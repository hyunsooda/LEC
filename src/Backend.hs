{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend where

import Prelude hiding (id)

import Type
import CFG
import Metadata
import Util

import System.Demangle.Pure (demangle)
import Data.ByteString.Short (ShortByteString)
import Data.List (isPrefixOf, isInfixOf, sort)
import Data.Word (Word16, Word32)
import Data.Maybe (isJust, fromJust)
import Data.Text.Lazy (unpack)
import qualified Data.Map as M
import Control.Monad.State hiding (void)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.CallingConvention as AST
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.AST.Global as AST
import qualified LLVM.AST.Operand as AST hiding (PointerType)

import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM
import LLVM.IRBuilder.Instruction as LLVM

import Prettyprinter hiding (line, line', column)
import LLVM.Pretty
import Debug.Trace

panic :: (Monad m, Pretty a) => String -> a -> m b
panic errStr expr = do
  traceM $ unpack $ ppll expr
  error errStr

defineGlobalStr :: (MonadIRBuilder m, MonadModuleBuilder m) => String -> String -> m C.Constant
defineGlobalStr varName value = LLVM.globalStringPtr varName (AST.mkName value)

emitGlobalAnsiStr :: (MonadIRBuilder m, MonadModuleBuilder m) => m ()
emitGlobalAnsiStr = do
  defineGlobalStr "B[37m" "LEC_ANSI_WHITE_g767akzwihq04k3frbvijvx2l5gdn0sk" >>
    defineGlobalStr "B[31m" "LEC_ANSI_RED_g767akzwihq04k3frbvijvx2l5gdn0sk" >>
      pure ()

getLocalOperandName :: AST.Operand -> AST.Name
getLocalOperandName (AST.LocalReference _ varName) = varName

constantToInt :: AST.Operand -> Integer
constantToInt (AST.ConstantOperand (C.Int {..})) = integerValue
constantToInt (AST.ConstantOperand (C.Null {})) = 0

getIntValue :: MonadState StateMap m => AST.Operand -> m Integer
getIntValue c@(AST.ConstantOperand _) = pure $ constantToInt c

getIntValue e@(AST.LocalReference _ name) = do
  memAllocPtrs <- gets intMap
  case M.lookup name memAllocPtrs of
    Just size -> pure size
    _ -> panic "ERR1" e

makeInt32Operand :: Integer -> AST.Operand
makeInt32Operand n = AST.ConstantOperand (C.Int 32 n)

makeInt64Operand :: Integer -> AST.Operand
makeInt64Operand n = AST.ConstantOperand (C.Int 64 n)

getVoidOperand :: AST.Operand
getVoidOperand = AST.ConstantOperand (C.Null AST.void)

typeSize :: AST.Type -> Either String Int
typeSize typ = case typ of
  AST.IntegerType {..} -> Right $ fromIntegral typeBits
  -- TODO: Consider multiple dimension
  AST.ArrayType   {..} -> typeSize elementType
  _ -> Left $ "Unimplemented type: " ++ show typ

updateIntVal :: MonadState StateMap m => AST.Name -> Integer -> m ()
updateIntVal varName val = modify $ \sm -> sm { intMap   = M.insert varName val (intMap sm) }

initTaintList :: MonadState StateMap m => AST.Name -> m ()
initTaintList sourceVarName = modify $ \sm -> sm { taintMap = M.insert sourceVarName [] (taintMap sm)}

updateTaintList :: MonadState StateMap m => AST.Name -> AST.Name -> m ()
updateTaintList sourceVarName varName = do
  tm <- gets taintMap
  let tainted = M.filterWithKey findVar tm
      (sourceVar', taintList) = M.elemAt 0 tainted
      updatedTaints = taintList ++ [varName]
   in
   case length tainted of
     0 -> modify $ \sm -> sm { taintMap = M.insert sourceVarName [] tm }
     1 -> modify $ \sm -> sm { taintMap = M.insert sourceVar' updatedTaints tm }
     _ -> pure ()
  where
    findVar allocaVar vars =
      if null vars then allocaVar == sourceVarName
                   else  allocaVar == sourceVarName || sourceVarName `elem` vars

voidRetTyp :: AST.Type
voidRetTyp = AST.FunctionType AST.void [] False

i64RetTyp :: AST.Type
i64RetTyp = AST.FunctionType AST.i64 [] False

libcExit :: AST.Operand
libcExit = AST.ConstantOperand (C.GlobalReference (AST.Name "exit"))

emitLibcExit :: MonadModuleBuilder m => m AST.Operand
emitLibcExit = LLVM.extern (AST.Name "exit") [AST.i32] AST.void

emitLibcPrintf :: MonadModuleBuilder m => m AST.Operand
emitLibcPrintf = LLVM.extern (AST.Name "printf") [AST.i32] AST.void

emitIntMapCount :: MonadModuleBuilder m => m AST.Operand
emitIntMapCount = LLVM.extern (AST.Name "_ZNKSt3mapIiiSt4lessIiESaISt4pairIKiiEEE5countERS3_") [AST.ptr, AST.ptr] AST.i64

libcPrintf :: AST.Operand
libcPrintf = AST.ConstantOperand (C.GlobalReference (AST.Name "printf"))

getGlobalFuncOp :: AST.Name -> AST.Operand
getGlobalFuncOp nm = AST.ConstantOperand (C.GlobalReference nm)

isNotDebugInstr :: MonadState StateMap m => AST.Named AST.Instruction -> m Bool
isNotDebugInstr (AST.Do AST.Call{function = func, arguments}) =
  case getFuncName func of
    Just name ->
      if name == "llvm.dbg.declare"
         then
           let (varMD, sourceMapMD) = (head arguments, arguments !! 1)
               varName = getVarName varMD
            in do
           varInfo <- getVarInfo sourceMapMD
           addVarInfo varName varInfo
           pure False
         else pure True
    _ -> pure True
isNotDebugInstr _ = pure True

installAssertI ::
  (MonadIO m, MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
    AST.Name -> [AST.Named AST.Instruction] ->
    AST.Named AST.Instruction -> m [AST.Named AST.Instruction]
installAssertI ctxName acc instr@(_ AST.:= AST.GetElementPtr { type' = AST.NamedTypeReference {}}) = pure $ acc ++ [instr]
installAssertI ctxName acc instr@(_ AST.:= AST.GetElementPtr { type' = AST.StructureType {}}) = pure $ acc ++ [instr]
installAssertI ctxName acc instr@(_ AST.:= AST.GetElementPtr {..}) = do
  memAllocPtrs <- gets intMap
  idx <- getIdx type'
  case getOperandName address of
    Just addrName ->
      case M.lookup addrName memAllocPtrs of
        Just size ->
         case typeSize type' of
            Right 32 -> do
              boundAssertCall <- callBoundAssert (size `div` 4) idx addrName metadata
              pure $ acc ++ [boundAssertCall, instr]
            _ -> pure $ acc ++ [instr]
        Nothing -> pure $ acc ++ [instr]
    _ -> pure $ acc ++ [instr]
  where
    getIdx AST.IntegerType {} = getIntValue (head indices)
    -- TODO: Consider multiple dimension
    getIdx AST.ArrayType {}   = getIntValue (last indices)
    getIdx _ = do
      traceM $ show type'
      panic "QWE" instr

-- TODO: Consider this handler. It would be handled when global `std::map` variable is used
installAssertI ctxName acc instr@(_ AST.:= AST.Call {..}) = do
  when (isMapAccess demangled) $
    case fnName of
      Just fnName' -> do
        ni <- nextInstr
        case ni of
          Just ni' ->
            when (not . isStoreInstr $ ni') $ do
              traceM $ "Test me : " ++ demangled
          _ -> pure ()
      _ -> pure ()
  pure $ acc ++ [instr]
    where
      fnName = getFuncName function
      demangled = getDemangledFuncName function
      isMapAccess fnName =
        "std::map<" `isPrefixOf` fnName && "::operator[]" `isInfixOf` fnName
      isStoreInstr (AST.Do AST.Store {}) = True
      isStoreInstr _ = False
      nextInstr = do
        nextInstrs <- getNextInstr ctxName $ iwrap instr
        if not . null $ nextInstrs
           then pure . Just . wrapi . head $ nextInstrs
           else pure Nothing

installAssertI _ acc instr = pure $ acc ++ [instr]

installAssertT :: (MonadIO m, MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
  AST.Name -> (AST.Named AST.Terminator) -> m (Maybe (AST.Named AST.Instruction))
installAssertT ctxName term@(varName AST.:= AST.Invoke { function' = func, .. }) = do
  if (isMapAccess demangled) then
    case fnNm of
      Just _ -> do
        ni <- nextInstr
        case ni of
          Just ni' ->
            if (not . isStoreInstr $ ni') then do
              let (mapObj, key) = getArgs arguments'
              mapUninitCall <- callMapUninitChecker mapObj key metadata'
              pure . Just $ mapUninitCall
            else
              pure Nothing
          _ -> pure Nothing
      Nothing -> pure Nothing
  else
    pure Nothing

  where
    fnNm = getFuncName func
    demangled = getDemangledFuncName func

    isMapAccess fnNm =
      "std::map<" `isPrefixOf` fnNm && "::operator[]" `isInfixOf` fnNm
    isMapCount fnNm =
      "std::map<" `isPrefixOf` fnNm && "::count(" `isInfixOf` fnNm
    isMapOperation fnNm = "std::map<" `isPrefixOf` fnNm

    isStoreInstr (AST.Do AST.Store {}) = True
    isStoreInstr _ = False

    nextInstr = do
      nextInstrs <- getNextInstr ctxName $ twrap term
      if not . null $ nextInstrs
         then pure . Just . wrapi . head $ nextInstrs
         else pure Nothing

    getArgs [(arg1, _), (arg2, _)] = (arg1, arg2)

installAssertT _ _ = pure Nothing

updateIntMap :: MonadState StateMap m => AST.Global -> m ()
updateIntMap f = do
  forM_ (AST.basicBlocks f) iterBB
    where
      iterBB (AST.BasicBlock _ instrs _) = mapM updateVar instrs
      updateVar (varName AST.:= callInstr@AST.Call {..}) = do
        ptr <- getMemAlloc callInstr
        case ptr of
          Right size -> modify $ \sm -> sm { intMap =  M.insert varName size (intMap sm)}
          _ -> pure ()
      updateVar expr@(AST.Do AST.Store {..}) = do
        memAllocPtrs <- gets intMap
        case (getOperandName address, getOperandName value) of
          (Just addrName, Just operandName) -> do
            case M.lookup operandName memAllocPtrs of
              Just size -> updateIntVal addrName size >> updateTaintList operandName addrName
              Nothing -> pure ()
          (Just addrName, Nothing) -> do
            val <- getIntValue value
            modify $ \sm -> sm { intMap =  M.insert addrName val (intMap sm)}
          _ -> panic "ERR5" expr
      updateVar (varName AST.:= AST.Alloca {..}) = do
        case allocatedType of
          AST.IntegerType {} -> initTaintList varName >> getElemLen varName numElements
          AST.PointerType {} -> initTaintList varName >> getElemLen varName numElements
          AST.ArrayType {..} -> do
            initTaintList varName
            modify $ \sm -> sm { intMap = M.insert varName (fromIntegral nArrayElements) (intMap sm)}
          _ -> pure ()
      updateVar expr@(varName AST.:= AST.Load {..}) = do
        memAllocPtrs <- gets intMap
        case getOperandName address of
          Just addrName -> do
            case M.lookup addrName memAllocPtrs of
              Just size -> updateIntVal varName size >> updateTaintList addrName varName
              Nothing -> pure ()
          _ -> panic "ERR6" expr
      updateVar expr@(varName AST.:= AST.GetElementPtr {..}) = do
        case getOperandName address of
          Just addrName -> updateTaintList addrName varName
          _ -> panic "ERR7" expr
      updateVar expr@(varName AST.:= AST.BitCast {..}) = do -- TODO: Refacotr me
        case getOperandName operand0 of
          Just operandName -> addToTracker varName operandName
          _ -> panic "ERR8" expr
      updateVar expr@(varName AST.:= AST.SExt{..}) = do -- TODO: Refactor me
        case getOperandName operand0 of
          Just operandName -> addToTracker varName operandName
          _ -> panic "ERR9" expr
      updateVar (varName AST.:= AST.Mul{..}) = do -- TODO: Refactor me (consider variable * variable)
        lhs <- getIntValue operand0
        rhs <- getIntValue operand1
        modify $ \sm -> sm { intMap =  M.insert varName (lhs * rhs) (intMap sm)}
      updateVar _ = pure ()

      addToTracker varName operandName = do
        memAllocPtrs <- gets intMap
        case M.lookup operandName memAllocPtrs of
          Just val -> updateIntVal varName val >> updateTaintList operandName varName
          _ -> pure ()

      getElemLen varName = \case
          Just c ->
            modify $ \sm -> sm { intMap = M.insert varName (constantToInt c) (intMap sm)}
          _ -> pure ()

getMemAlloc :: (MonadState StateMap m) => AST.Instruction -> m (Either String Integer)
getMemAlloc expr@(AST.Call {arguments, function = func}) =
  case func of
    Right (AST.ConstantOperand (C.GlobalReference (AST.Name funcName))) ->
      if funcName == "malloc"
         then do
           val <- getIntValue . fst . head $ arguments
           pure $ Right val
         else pure $ Left "Error: Expected one argument"
    Left _ -> pure $ Left "Error: not a malloc"
    _ -> panic "ERR10" expr

instrument ::
  (MonadIO m, MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
    AST.Global -> m AST.Definition
instrument f@(AST.Function { name = fn }) = do
  let bbs = AST.basicBlocks f
  if null bbs
     then pure $ AST.GlobalDefinition f
     else do
       newBBs <- mapM filterDbgInstr bbs
       instrumented <- mapM addBoundAssert newBBs
       pure $ AST.GlobalDefinition $ f { AST.basicBlocks = instrumented }
  where
    filterDbgInstr (AST.BasicBlock name instrs term) = do
      newInstrs <- filterM isNotDebugInstr instrs
      pure $ AST.BasicBlock name newInstrs term

    addBoundAssert (AST.BasicBlock name instrs term) = do
      instrWithAssert <- foldM (installAssertI fn) [] instrs
      termWithassert <- installAssertT fn term
      case termWithassert of
        Just termAssert -> pure $ AST.BasicBlock name (instrWithAssert ++ [termAssert]) term
        Nothing    -> pure $ AST.BasicBlock name instrWithAssert term

outOfBoundErrAssertFmt :: IRBuilderT Env ()
outOfBoundErrAssertFmt =
  let oobFormatter = [mtstr|%sFound out of bound access: [%s:%d:%d]%s
    array length: %d, indexed by: %d
    variable name: %s, allocated at: %d
|] in do
  defineGlobalStr oobFormatter "OUT_OF_BOUND_ASSERT_FMT" >> pure ()

mapKeyExistAssertFmt :: IRBuilderT Env ()
mapKeyExistAssertFmt =
  let oobFormatter = [mtstr|%sFound unitialized key access: [%s:%d:%d]%s
    variable name: %s, allocated at: %d
|] in do
  defineGlobalStr oobFormatter "MAP_UNINIT_ACCESS_ASSERT_FMT" >> pure ()

callMapUninitChecker
  :: (MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
    AST.Operand
    -> AST.Operand
    -> [(a1, AST.MDRef a2)]
    -> m (AST.Named AST.Instruction)
callMapUninitChecker mapObj key metadata = do
  (varName, _, allocatedLine) <- getVarSource (fromJust . getOperandName $ mapObj)
  (accessFileName, _, accessLine, accessCol) <- getMDScope metadata
  let accessLine' = makeInt32Operand accessLine
      accessCol' = makeInt32Operand accessCol
      allocatedLine' = makeInt32Operand allocatedLine
   in do
     accessFileName' <- LLVM.globalStringPtr accessFileName (AST.Name "FILE_NAME")
     varName' <- LLVM.globalStringPtr varName (AST.Name "VAR_NAME")
     pure $ AST.Do
       AST.Call {
        tailCallKind = Nothing,
        AST.callingConvention = AST.C,
        AST.returnAttributes = [],
        AST.type' = i64RetTyp,
        AST.function = Right (AST.ConstantOperand (C.GlobalReference (AST.Name "LEC_callMapCnt"))),
        AST.arguments = [
          (mapObj, []), (key, []),
          (accessLine', []), (accessCol', []),
          (AST.ConstantOperand accessFileName', []),
          (AST.ConstantOperand varName', []), (allocatedLine', [])
        ],
        AST.functionAttributes = [],
        AST.metadata = []
      }

defMapUninitChecker :: AST.Name -> IRBuilderT Env AST.Operand
defMapUninitChecker funcNm = mdo
-- defMapUninitChecker :: (MonadIO m, MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
--   AST.Name -> m (IRBuilderT Env AST.Operand)
-- defMapUninitChecker funcNm = mdo

-- define void @LEC_callMapCnt(ptr %mapObj_0, ptr %key_0, i32 %line_0, i32 %col_0, ptr %fileName_0) {
--   %1 = call i64 @_ZNKSt3mapIiiSt4lessIiESaISt4pairIKiiEEE5countERS3_(ptr %mapObj_0, ptr %key_0)
--   %2 = icmp eq i64 %1, 0
--   br i1 %2, label %panic_0, label %ret_0

-- panic_0:                                          ; preds = %0
--   call void @printf(ptr @MAP_UNINIT_ACCESS_ASSERT_FMT, ptr @LEC_ANSI_RED_g767akzwihq04k3frbvijvx2l5gdn0sk, ptr %fileName_0, i32 %line_0, i32 %col_0)
--   call void @exit(i32 1)
--   ret void

-- ret_0:                                            ; preds = %0
--   ret void

  let mapObj = (AST.ptr, LLVM.ParameterName "mapObj")
      key = (AST.ptr, LLVM.ParameterName "key")
      line = (AST.i32, LLVM.ParameterName "line")
      col = (AST.i32, LLVM.ParameterName "col")
      fileName = (AST.ptr, LLVM.ParameterName "fileName")
      varName = (AST.ptr, LLVM.ParameterName "varName")
      keyName = (AST.ptr, LLVM.ParameterName "keyName")
  LLVM.function "LEC_callMapCnt" [mapObj, key, line, col, fileName, varName, keyName] AST.void body
    where
      zero = makeInt64Operand 0
      body [mapObj', key', line', col', fileName', varName', keyName'] = mdo
        keyCount <- LLVM.call i64RetTyp (getGlobalFuncOp funcNm) [
          (AST.LocalReference AST.ptr (getLocalOperandName mapObj'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName key'), [])]
        isCountZero <- LLVM.icmp AST.EQ keyCount zero
        LLVM.condBr isCountZero panic ret
        panic <- LLVM.block `LLVM.named` "panic"
        _ <- LLVM.call voidRetTyp libcPrintf [
          (AST.ConstantOperand (C.GlobalReference (AST.Name "MAP_UNINIT_ACCESS_ASSERT_FMT")), []),
          (AST.ConstantOperand (C.GlobalReference (AST.Name "LEC_ANSI_RED_g767akzwihq04k3frbvijvx2l5gdn0sk")), []),
          (AST.LocalReference AST.ptr (getLocalOperandName fileName'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName line'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName col'), []),
          (AST.ConstantOperand (C.GlobalReference (AST.Name "LEC_ANSI_WHITE_g767akzwihq04k3frbvijvx2l5gdn0sk")), []),
          (AST.LocalReference AST.ptr (getLocalOperandName varName'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName keyName'), [])]
        _ <- LLVM.call voidRetTyp libcExit [(makeInt32Operand 1, [])]
        ret <- LLVM.block `LLVM.named` "ret"
        pure ()

callBoundAssert
  :: (MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
    Integer
    -> Integer
    -> AST.Name
    -> [(a1, AST.MDRef a2)]
    -> m (AST.Named AST.Instruction)
callBoundAssert arrSize idx targetAddrName metadata = do
  (varName, _, allocatedLine) <- getVarSource targetAddrName
  (accessFileName, _, accessLine, accessCol) <- getMDScope metadata
  let arrSize' = makeInt32Operand arrSize
      idx' = makeInt32Operand idx
      accessLine' = makeInt32Operand accessLine
      accessCol' = makeInt32Operand accessCol
      allocatedLine' = makeInt32Operand allocatedLine
   in do
     accessFileName' <- LLVM.globalStringPtr accessFileName (AST.Name "FILE_NAME")
     varName' <- LLVM.globalStringPtr varName (AST.Name "VAR_NAME")
     pure $ AST.Do
       AST.Call {
        tailCallKind = Nothing,
        AST.callingConvention = AST.C,
        AST.returnAttributes = [],
        AST.type' = voidRetTyp,
        AST.function = Right (AST.ConstantOperand (C.GlobalReference (AST.Name "LEC_boundAssertion"))),
        AST.arguments = [
          (arrSize', []), (idx', []), (accessLine', []), (accessCol', []),
          (AST.ConstantOperand accessFileName', []),
          (AST.ConstantOperand varName', []), (allocatedLine', [])
        ],
        AST.functionAttributes = [],
        AST.metadata = []
      }

defBoundChecker :: IRBuilderT Env AST.Operand
defBoundChecker = mdo
  let arrSize = (AST.i32, LLVM.ParameterName "arrSize")
      idx = (AST.i32, LLVM.ParameterName "idx")
      line = (AST.i32, LLVM.ParameterName "line")
      col = (AST.i32, LLVM.ParameterName "col")
      fileName = (AST.ptr, LLVM.ParameterName "fileName")
      varName = (AST.ptr, LLVM.ParameterName "varName")
      allocatedLine = (AST.ptr, LLVM.ParameterName "allocatedLine")

  LLVM.function "LEC_boundAssertion" [arrSize, idx, line, col, fileName, varName, allocatedLine] AST.void body
    where
      body [arrSize', idx', line', col', fileName', varName', allocatedLine'] =  mdo
        outOfBoundAccess <- LLVM.icmp AST.SGT idx' arrSize'
        LLVM.condBr outOfBoundAccess panic ret
        panic <- LLVM.block `LLVM.named` "panic"
        _ <- LLVM.call voidRetTyp libcPrintf [
          -- TODO: Make `OUT_OF_BOUND_ASSERT_FMT` as global constant
          (AST.ConstantOperand (C.GlobalReference (AST.Name "OUT_OF_BOUND_ASSERT_FMT")), []),
          (AST.ConstantOperand (C.GlobalReference (AST.Name "LEC_ANSI_RED_g767akzwihq04k3frbvijvx2l5gdn0sk")), []),
          (AST.LocalReference AST.ptr (getLocalOperandName fileName'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName line'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName col'), []),
          (AST.ConstantOperand (C.GlobalReference (AST.Name "LEC_ANSI_WHITE_g767akzwihq04k3frbvijvx2l5gdn0sk")), []),
          (AST.LocalReference AST.ptr (getLocalOperandName arrSize'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName idx'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName varName'), []),
          (AST.LocalReference AST.ptr (getLocalOperandName allocatedLine'), [])]
        _ <- LLVM.call voidRetTyp libcExit [(makeInt32Operand 1, [])]
        ret <- LLVM.block `LLVM.named` "ret"
        pure ()
