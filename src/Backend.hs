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
import OpUtil
import Emit

import System.Demangle.Pure (demangle)
import Data.ByteString.Short (ShortByteString)
import Data.Word (Word16, Word32)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M
import Control.Monad.State hiding (void)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.CallingConvention as AST
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.AST.Global as AST
import qualified LLVM.AST.Operand as AST hiding (PointerType)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.ParameterAttribute as PA (ParameterAttribute)

import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM
import LLVM.IRBuilder.Instruction as LLVM

import Debug.Trace

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

getGlobalFuncOp :: AST.Name -> AST.Operand
getGlobalFuncOp nm = AST.ConstantOperand (C.GlobalReference nm)

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
  installable <- installUMAChecker ctxName function (iwrap instr) arguments metadata
  case installable of
    Just checkerCall -> pure $ acc ++ [checkerCall, instr]
    Nothing -> pure $ acc ++ [instr]
installAssertI _ acc instr = pure $ acc ++ [instr]

installAssertT :: (MonadIO m, MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
  AST.Name -> (AST.Named AST.Terminator) -> m (Maybe (AST.Named AST.Instruction))
installAssertT ctxName term@(varName AST.:= AST.Invoke { function' = func, .. }) = do
  installUMAChecker ctxName func (twrap term) arguments' metadata'
installAssertT _ _ = pure Nothing

installUMAChecker ::
  (MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
    AST.Name ->
    AST.CallableOperand ->
    InstructionWrap ->
    [(AST.Operand, [PA.ParameterAttribute])] ->
    AST.InstructionMetadata ->
    m (Maybe (AST.Named AST.Instruction))
installUMAChecker ctxName func wInstr arguments metadata =
  if (isMapAccess demangled) then
    case fnNm of
      Just _ -> do
        ni <- nextInstr
        case ni of
          Just ni' ->
            if (not . isStoreInstr $ ni') then do
              let (mapObj, key) = getArgs arguments
              mapUninitCall <- callMapUninitChecker mapObj key metadata
              pure . Just $ mapUninitCall
            else
              ret
          _ -> ret
      Nothing -> ret
  else
    ret
  where
    ret = pure Nothing
    fnNm = getFuncName func
    demangled = getDemangledFuncName func

    nextInstr = do
      nextInstrs <- getNextInstr ctxName $ wInstr
      if not . null $ nextInstrs
        then pure . Just . wrapi . head $ nextInstrs
        else pure Nothing

    getArgs [(arg1, _), (arg2, _)] = (arg1, arg2)

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
      newInstrs <- filterM filterDebugInfo instrs
      pure $ AST.BasicBlock name newInstrs term

    addBoundAssert (AST.BasicBlock name instrs term) = do
      instrWithAssert <- foldM (installAssertI fn) [] instrs
      termWithassert <- installAssertT fn term
      case termWithassert of
        Just termAssert -> pure $ AST.BasicBlock name (instrWithAssert ++ [termAssert]) term
        Nothing    -> pure $ AST.BasicBlock name instrWithAssert term
