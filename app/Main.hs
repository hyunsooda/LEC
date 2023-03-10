{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude hiding (readFile, writeFile)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Data.List (isSuffixOf, isPrefixOf)
import Data.ByteString (readFile, writeFile, ByteString)
import Data.Word (Word16, Word32)
import qualified Data.Map as M
import Control.Monad.State hiding (void)
import Control.Monad.RWS hiding (void)

import LLVM.AST as IR
import LLVM.AST.Name as IR
import LLVM.AST.Type as IR
import LLVM.AST.Global as G
import LLVM.AST.Constant as C
import LLVM.AST.Instruction as IR
import LLVM.Internal.Context as LLVM
import LLVM.Internal.Module as LLVM
import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM
import LLVM.IRBuilder.Instruction as LLVM

import LLVM.AST.CallingConvention as CC
import LLVM.AST.AddrSpace as IR
import System.Console.ANSI
import qualified LLVM.AST.IntegerPredicate     as IP
import qualified LLVM.AST.Operand as OP

data StateMap = StateMap { intMap :: M.Map Name Integer
                         , debugMap :: M.Map MetadataNodeID MDNode
                         , sourceMap :: M.Map Name OP.DIVariable
                         } deriving Show

type HaskellPassInput = [String]
type HaskellPassState = StateMap
type HaskellPassOutput = [String]
type Env = (RWST HaskellPassInput HaskellPassOutput HaskellPassState ModuleBuilder)
type HaskellPass a = IR.Module -> IRBuilderT Env a

runHaskellPass :: (HaskellPass a) -> HaskellPassInput -> IR.Module -> IO (IR.Module, a)
runHaskellPass pass input mod = do
  let haskellPassState = StateMap { intMap = M.empty, debugMap = M.empty, sourceMap = M.empty }
  let irBuilderState = LLVM.emptyIRBuilder
  let modBuilderState = LLVM.emptyModuleBuilder
  let (((result, _), output), defs) = LLVM.runModuleBuilder modBuilderState $ do
                              (\x -> evalRWST x input haskellPassState) $
                                runIRBuilderT irBuilderState $ pass mod

  mapM_ (\log -> if isPrefixOf "ERROR: " log then printErr log else putStrLn log) output
  return (mod { IR.moduleDefinitions = (defProver:defs) }, result)


getOperandName :: Operand -> Maybe Name
getOperandName (LocalReference _ varName) = Just varName
getOperandName (ConstantOperand (C.GlobalReference varName)) = Just varName
getOperandName _ = Nothing

getLocalOperandName :: Operand -> Name
getLocalOperandName (LocalReference _ varName) = varName

constantToInt :: IR.Operand -> Integer
constantToInt (ConstantOperand (C.Int {..})) = integerValue

makeInt32Operand :: Integer -> IR.Operand
makeInt32Operand n = ConstantOperand (C.Int 32 n)

getFuncName :: CallableOperand -> Maybe String
getFuncName =
  \case
    Right (ConstantOperand (C.GlobalReference (Name funcName))) -> Just . show $ funcName
    _ -> Nothing

getMDNodeID :: MDRef a -> MetadataNodeID
getMDNodeID (MDRef id) = id
getMDNodeID _ = undefined

getMDLocation :: MDNode -> (Word32, Word16, MDRef OP.DILocalScope)
getMDLocation (DILocation (OP.Location {..})) = (line, column, scope)

getMD :: MonadState StateMap m => MDRef a -> m MDNode
getMD mdRef = do
  md <- gets debugMap
  pure $ (M.!) md $ getMDNodeID mdRef

getVarName :: (Operand, a) -> Name
getVarName (MetadataOperand (MDValue (LocalReference _ varName)), _) = varName
getVarName _ = undefined

getVarInfo :: MonadState StateMap m => (Operand, a) -> m OP.DINode
getVarInfo (MetadataOperand (MDNode mdRef), _) = do
  md <- getMD mdRef
  pure $ diVar md
    where diVar (DINode var) = var

addVarInfo :: MonadState StateMap m => Name -> OP.DINode -> m ()
addVarInfo name (OP.DIVariable var) = do
  modify $ \sm -> sm { sourceMap = M.insert name var (sourceMap sm) }

getMDFuncFileNames :: MonadState StateMap m => MDNode -> m (String, String)
getMDFuncFileNames (DINode (OP.DIScope (OP.DILocalScope (OP.DISubprogram (OP.Subprogram {..}))))) = do
  fileName' <- getFileName file
  pure (fileName', funcName)
    where funcName = show name

getMDScope [(_, MDRef id)] = do
  md <- gets debugMap
  let (line, col, mdRef) = getMDLocation $ (M.!) md id
      mdScope = (M.!) md $ getMDNodeID mdRef
   in do
   (fileName, funcName) <- getMDFuncFileNames mdScope
   pure (fileName, funcName, fromIntegral line, fromIntegral col)

typeSize :: Type -> Either String Int
typeSize typ = case typ of
  IR.IntegerType {..} -> Right $ fromIntegral typeBits
  _ -> Left $ "Unimplemented type: " ++ show typ

printErr :: String -> IO ()
printErr err =
  setSGR [SetColor Foreground Vivid Red] >> print err >> setSGR [Reset]

voidRetTyp :: Type
voidRetTyp = (FunctionType IR.void [] False)

libcExit :: IR.Operand
libcExit = ConstantOperand (C.GlobalReference (Name "exit"))

libcPrintf :: IR.Operand
libcPrintf = ConstantOperand (C.GlobalReference (Name "printf"))

getFileName :: MonadState StateMap m => Maybe (MDRef a) -> m String
getFileName (Just mdRef) = do
  md <- getMD mdRef
  pure $ fileName md
  where
    fileName (OP.DINode (OP.DIScope (OP.DIFile (OP.File {..})))) = show filename
getFileName _ = undefined

getVarSource :: (MonadState StateMap m, Num c) => p -> m (String, String, c)
getVarSource targetVarName = do
  sourceInfo <- gets sourceMap
  vals <- gets intMap
  let toBeFixed = fst . head $ M.toList vals
      -- TODO: Fix me
      -- source = (M.!) sourceInfo targetVarName in
      source = (M.!) sourceInfo toBeFixed in
      getSourceInfo source
  where
    getSourceInfo (OP.DILocalVariable (OP.LocalVariable {..})) = do
      fileName' <- getFileName file
      pure $ (show name, fileName', fromIntegral line)

isDbgInstr :: MonadState StateMap m => Named Instruction -> m Bool
isDbgInstr (Do call@Call{..}) =
  case getFuncName function of
    Just name ->
      if name == "\"llvm.dbg.declare\""
         then
         let (varMD, sourceMapMD) = (arguments !! 0, arguments !! 1)
             varName = getVarName varMD
          in do
         varInfo <- getVarInfo sourceMapMD
         addVarInfo varName varInfo
         pure False
         else pure True
    _ -> pure True
isDbgInstr _ = pure True


getElemPtrInstr :: (MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
  [Named Instruction] -> Named Instruction -> m [Named Instruction]
getElemPtrInstr acc instr@(varName := getElemPtr@IR.GetElementPtr {..}) = do
  memAllocPtrs <- gets intMap
  md <- gets debugMap
  case getOperandName address of
    Just addrName ->
      case M.lookup addrName memAllocPtrs of
        Just size ->
          let idx = constantToInt . head $ indices
              mdID = getMDNodeID $ snd . head $ metadata
           in
           case typeSize type' of
              Right 32 -> do
                boundAssertCall <- callBoundAssert (size `div` 4) idx addrName metadata
                pure $ acc ++ [boundAssertCall, instr]
              _ -> pure $ acc ++ [instr]
        Nothing -> pure $ acc ++ [instr]
    _ -> pure $ acc ++ [instr]
getElemPtrInstr acc instr = pure $ acc ++ [instr]


updateIntMap :: MonadState StateMap m => Global -> m ()
updateIntMap f = do
  forM_ (G.basicBlocks f) iterBB
    where
      iterBB bb@(G.BasicBlock name instrs term) = mapM updateVar instrs
      updateVar instr@(varName := call@Call {}) =
        case getMemAlloc call of
          Right size -> modify $ \sm -> sm { intMap =  M.insert varName size (intMap sm)}
          _ -> pure ()
      updateVar instr@(Do Store {..}) = do
        memAllocPtrs <- gets intMap
        case (getOperandName address, getOperandName value) of
          (Just addrName, Just operandName) -> do
            case M.lookup operandName memAllocPtrs of
              Just size -> modify $ \sm -> sm { intMap =  M.insert addrName size (intMap sm)}
              Nothing -> pure ()
          _ -> pure ()
      updateVar instr@(varName := Load {..}) = do -- TODO: Refactoring me
        memAllocPtrs <- gets intMap
        case getOperandName address of
          Just addrName -> do
            case M.lookup addrName memAllocPtrs of
              Just size -> modify $ \sm -> sm { intMap =  M.insert varName size (intMap sm)}
              Nothing -> pure ()
          _ -> pure ()
      updateVar instr@(varName := IR.BitCast {..}) = do
        case getOperandName operand0 of
          Just operandName -> addToTracker varName operandName
          _ -> pure ()
      updateVar _ = pure ()

      addToTracker varName operandName = do
        memAllocPtrs <- gets intMap
        case M.lookup operandName memAllocPtrs of
          Just memSize -> modify $ \sm -> sm { intMap =  M.insert varName memSize (intMap sm) }
          _ -> pure ()

getMemAlloc :: Instruction -> Either String Integer
getMemAlloc (Call {..}) =
  case function of
    Right (ConstantOperand (C.GlobalReference (Name funcName))) ->
      if funcName == "malloc"
         then (Right . constantToInt . fst . head) arguments
         else Left "Error: Expected one argument"
    Left _ -> Left "Error: not a malloc"


instrument :: (MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) => Global -> m Definition
instrument f = do
  let bbs = G.basicBlocks f
  if length bbs == 0
     then pure $ IR.GlobalDefinition f
     else do
       newBBs <- mapM filterDbgInstr bbs
       instrumented <- mapM addBoundAssert newBBs
       pure $ IR.GlobalDefinition $ f { basicBlocks = instrumented }
  where
    filterDbgInstr bb@(G.BasicBlock name instrs term) = do
      newInstrs <- filterM isDbgInstr instrs
      pure $ G.BasicBlock name newInstrs term

    addBoundAssert bb@(G.BasicBlock name instrs term) = do
      newInstrs <- foldM getElemPtrInstr [] instrs
      pure $ G.BasicBlock name newInstrs term


helloWorldPass :: HaskellPass ()
helloWorldPass mod = do
  let defs = IR.moduleDefinitions mod
  defBoundChecker
  mapM_ iterDef (reverse defs)
  addGlobalString >> outOfBoundErrLogFormat >> return ()
  where
    iterDef d@(IR.GlobalDefinition f@(Function {})) = do
      updateIntMap f
      instrumented <- instrument f
      LLVM.emitDefn instrumented
    iterDef d@(IR.MetadataNodeDefinition nodeId mdNode) = do
      modify $ \sm -> sm { debugMap = M.insert nodeId mdNode (debugMap sm) }
      LLVM.emitDefn d
    iterDef d = LLVM.emitDefn d

addGlobalString :: IRBuilderT Env ()
addGlobalString = do
  LLVM.globalStringPtr "this is instrumented string\n" (Name "globalProverStr")
  pure ()

outOfBoundErrLogFormat :: IRBuilderT Env ()
outOfBoundErrLogFormat = do
  LLVM.globalStringPtr "Found out of bound access: [%s:%d:%d]: \n\t array size: %d, indexed by: %d \n \t variable name: %s, allocated at: %d\n" (Name "OUT_OF_BOUND_LOG_FORMAT")
  pure ()

callProver :: Named Instruction
callProver =
  Do
    Call {
      tailCallKind = Nothing,
      IR.callingConvention = CC.C,
      IR.returnAttributes = [],
      IR.type' = voidRetTyp,
      IR.function = Right (ConstantOperand (C.GlobalReference (Name "prover"))),
      IR.arguments = [],
      IR.functionAttributes = [],
      IR.metadata = []
    }

callBoundAssert :: (MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
  Integer -> Integer -> p -> [(a1, MDRef a2)] -> m (Named Instruction)
callBoundAssert arrSize idx targetAddrName metadata = do
  (varName, allocatedFileName, allocatedLine) <- getVarSource targetAddrName
  (accessFileName, funcName, accessLine, accessCol) <- getMDScope metadata
  let arrSize' = makeInt32Operand arrSize
      idx' = makeInt32Operand idx
      accessLine' = makeInt32Operand accessLine
      accessCol' = makeInt32Operand accessCol
      allocatedLine' = makeInt32Operand allocatedLine
   in do
     accessFileName' <- LLVM.globalStringPtr accessFileName (Name "FILE_NAME")
     varName' <- LLVM.globalStringPtr varName (Name "VAR_NAME")
     pure $ Do
       Call {
        tailCallKind = Nothing,
        IR.callingConvention = CC.C,
        IR.returnAttributes = [],
        IR.type' = voidRetTyp,
        IR.function = Right (ConstantOperand (C.GlobalReference (Name "boundAssertion"))),
        IR.arguments = [
          (arrSize', []), (idx', []), (accessLine', []), (accessCol', []),
          (ConstantOperand accessFileName', []),
          (ConstantOperand varName', []), (allocatedLine', [])
        ],
        IR.functionAttributes = [],
        IR.metadata = []
      }

defBoundChecker :: IRBuilderT Env Operand
defBoundChecker = mdo
  let arrSize = (IR.i32, LLVM.ParameterName "arrSize")
      idx = (IR.i32, LLVM.ParameterName "idx")
      line = (IR.i32, LLVM.ParameterName "line")
      col = (IR.i32, LLVM.ParameterName "col")
      fileName = (IR.ptr, LLVM.ParameterName "fileName")
      varName = (IR.ptr, LLVM.ParameterName "varName")
      allocatedLine = (IR.ptr, LLVM.ParameterName "allocatedLine")

  LLVM.function "boundAssertion" [arrSize, idx, line, col, fileName, varName, allocatedLine] IR.void body
    where
      body ops@(arrSize' : idx' : line' : col' : fileName' : varName' : allocatedLine' : []) =  mdo
        outOfBoundAccess <- LLVM.icmp IP.SGT idx' arrSize'
        LLVM.condBr outOfBoundAccess panic ret
        panic <- LLVM.block `LLVM.named` "panic"
        do
          LLVM.call voidRetTyp libcPrintf [
            (ConstantOperand (C.GlobalReference (Name "OUT_OF_BOUND_LOG_FORMAT")), []),
            (LocalReference IR.ptr (getLocalOperandName fileName'), []),
            (LocalReference IR.ptr (getLocalOperandName line'), []),
            (LocalReference IR.ptr (getLocalOperandName col'), []),
            (LocalReference IR.ptr (getLocalOperandName arrSize'), []),
            (LocalReference IR.ptr (getLocalOperandName idx'), []),
            (LocalReference IR.ptr (getLocalOperandName varName'), []),
            (LocalReference IR.ptr (getLocalOperandName allocatedLine'), [])]
          LLVM.call voidRetTyp libcExit [(makeInt32Operand 1, [])]
        ret <- LLVM.block `LLVM.named` "ret"
        pure ()

defProver :: Definition
defProver = GlobalDefinition functionDefaults
  { name = Name "prover"
  , parameters = ([], False)
  , returnType = IR.void
  , basicBlocks = [body]
  }
  where
    ptrTyp = IR.PointerType (IR.AddrSpace 0)
    body = BasicBlock
        (UnName 1)
        [
          UnName 1 := Load {
            IR.volatile = False,
            IR.type' = IR.ptr,
            IR.address = ConstantOperand (C.GlobalReference "globalProverStr"),
            IR.maybeAtomicity = Nothing,
            IR.alignment = 0,
            IR.metadata = []
                              },
          Do
            Call {
              tailCallKind = Nothing,
              IR.callingConvention = CC.C,
              IR.returnAttributes = [],
              IR.type' = FunctionType IR.void [] False,
              IR.function = Right (ConstantOperand (C.GlobalReference (Name "printf"))),
              IR.arguments = [(ConstantOperand (C.GlobalReference (Name "globalProverStr")), [])],
              IR.functionAttributes = [],
              IR.metadata = []
            }
        ]
        (Do $ Ret Nothing [])

main :: IO ()
main = do
  args <- getArgs
  if (length args) == 1
  then do
    let file = head args
    if isSuffixOf ".bc" file
    then do
      mod <- LLVM.withContext (\ctx -> do
               LLVM.withModuleFromBitcode ctx (LLVM.File file) LLVM.moduleAST)
      (mod', _) <- runHaskellPass helloWorldPass [] mod
      LLVM.withContext $ \ctx ->
        LLVM.withModuleFromAST
        ctx
        mod'
        (\modl -> LLVM.writeBitcodeToFile (LLVM.File "HELLO.bc") modl)

    else if isSuffixOf ".ll" file
    then do
      fcts <- readFile file
      mod <- LLVM.withContext (\ctx -> do
               LLVM.withModuleFromLLVMAssembly ctx fcts LLVM.moduleAST)
      (mod', _) <- runHaskellPass helloWorldPass [] mod
      LLVM.withContext $ \ctx ->
        LLVM.withModuleFromAST
        ctx
        mod'
        (\modl -> LLVM.writeLLVMAssemblyToFile (LLVM.File "output.ll") modl)

    else do
      putStrLn ("Invalid file extension (need either .bc or .ll): " ++ file)
      exitFailure

  else do
    putStrLn $ "usage: HelloWorld FILE.{bc,ll}"
    exitFailure
      where
        outputPath = "output.ll"
