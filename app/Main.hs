{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
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
import System.Console.ANSI

import qualified LLVM.AST as AST
import qualified LLVM.AST.Name as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.CallingConvention as AST
import qualified LLVM.AST.AddrSpace as AST
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.AST.Operand as AST hiding (PointerType)

import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C

import LLVM.Internal.Context as LLVM
import LLVM.Internal.Module as LLVM
import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM
import LLVM.IRBuilder.Instruction as LLVM

data StateMap = StateMap { intMap :: M.Map AST.Name Integer
                         , debugMap :: M.Map AST.MetadataNodeID AST.MDNode
                         , sourceMap :: M.Map AST.Name AST.DIVariable
                         } deriving Show

type HaskellPassInput = [String]
type HaskellPassState = StateMap
type HaskellPassOutput = [String]
type Env = (RWST HaskellPassInput HaskellPassOutput HaskellPassState ModuleBuilder)
type HaskellPass a = AST.Module -> IRBuilderT Env a

runHaskellPass :: (HaskellPass a) -> HaskellPassInput -> AST.Module -> IO (AST.Module, a)
runHaskellPass pass input mod = do
  let haskellPassState = StateMap { intMap = M.empty, debugMap = M.empty, sourceMap = M.empty }
  let irBuilderState = LLVM.emptyIRBuilder
  let modBuilderState = LLVM.emptyModuleBuilder
  let (((result, _), output), defs) = LLVM.runModuleBuilder modBuilderState $ do
                              (\x -> evalRWST x input haskellPassState) $
                                runIRBuilderT irBuilderState $ pass mod

  mapM_ (\log -> if isPrefixOf "ERROR: " log then printErr log else putStrLn log) output
  return (mod { AST.moduleDefinitions = (defProver:defs) }, result)


getOperandName :: AST.Operand -> Maybe AST.Name
getOperandName (AST.LocalReference _ varName) = Just varName
getOperandName (AST.ConstantOperand (C.GlobalReference varName)) = Just varName
getOperandName _ = Nothing

getLocalOperandName :: AST.Operand -> AST.Name
getLocalOperandName (AST.LocalReference _ varName) = varName

constantToInt :: AST.Operand -> Integer
constantToInt (AST.ConstantOperand (C.Int {..})) = integerValue

makeInt32Operand :: Integer -> AST.Operand
makeInt32Operand n = AST.ConstantOperand (C.Int 32 n)

getFuncName :: AST.CallableOperand -> Maybe String
getFuncName =
  \case
    Right (AST.ConstantOperand (C.GlobalReference (AST.Name funcName))) -> Just . show $ funcName
    _ -> Nothing

getMDNodeID :: AST.MDRef a -> AST.MetadataNodeID
getMDNodeID (AST.MDRef id) = id
getMDNodeID _ = undefined

getMDLocation :: AST.MDNode -> (Word32, Word16, AST.MDRef AST.DILocalScope)
getMDLocation (AST.DILocation (AST.Location {..})) = (line, column, scope)

getMD :: MonadState StateMap m => AST.MDRef a -> m AST.MDNode
getMD mdRef = do
  md <- gets debugMap
  pure $ (M.!) md $ getMDNodeID mdRef

getVarName :: (AST.Operand, a) -> AST.Name
getVarName (AST.MetadataOperand (AST.MDValue (AST.LocalReference _ varName)), _) = varName
getVarName _ = undefined

getVarInfo :: MonadState StateMap m => (AST.Operand, a) -> m AST.DINode
getVarInfo (AST.MetadataOperand (AST.MDNode mdRef), _) = do
  md <- getMD mdRef
  pure $ diVar md
    where diVar (AST.DINode var) = var

addVarInfo :: MonadState StateMap m => AST.Name -> AST.DINode -> m ()
addVarInfo name (AST.DIVariable var) = do
  modify $ \sm -> sm { sourceMap = M.insert name var (sourceMap sm) }

getMDFuncFileNames :: MonadState StateMap m => AST.MDNode -> m (String, String)
getMDFuncFileNames (AST.DINode (AST.DIScope (AST.DILocalScope (AST.DISubprogram (AST.Subprogram {..}))))) = do
  fileName' <- getFileName file
  pure (fileName', funcName)
    where funcName = show name

getMDScope [(_, AST.MDRef id)] = do
  md <- gets debugMap
  let (line, col, mdRef) = getMDLocation $ (M.!) md id
      mdScope = (M.!) md $ getMDNodeID mdRef
   in do
   (fileName, funcName) <- getMDFuncFileNames mdScope
   pure (fileName, funcName, fromIntegral line, fromIntegral col)

typeSize :: AST.Type -> Either String Int
typeSize typ = case typ of
  AST.IntegerType {..} -> Right $ fromIntegral typeBits
  _ -> Left $ "Unimplemented type: " ++ show typ

printErr :: String -> IO ()
printErr err =
  setSGR [SetColor Foreground Vivid Red] >> print err >> setSGR [Reset]

voidRetTyp :: AST.Type
voidRetTyp = (AST.FunctionType AST.void [] False)

libcExit :: AST.Operand
libcExit = AST.ConstantOperand (C.GlobalReference (AST.Name "exit"))

libcPrintf :: AST.Operand
libcPrintf = AST.ConstantOperand (C.GlobalReference (AST.Name "printf"))

getFileName :: MonadState StateMap m => Maybe (AST.MDRef a) -> m String
getFileName (Just mdRef) = do
  md <- getMD mdRef
  pure $ fileName md
  where
    fileName (AST.DINode (AST.DIScope (AST.DIFile (AST.File {..})))) = show filename
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
    getSourceInfo (AST.DILocalVariable (AST.LocalVariable {..})) = do
      fileName' <- getFileName file
      pure $ (show name, fileName', fromIntegral line)

isDbgInstr :: MonadState StateMap m => AST.Named AST.Instruction -> m Bool
isDbgInstr (AST.Do call@AST.Call{..}) =
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
  [AST.Named AST.Instruction] -> AST.Named AST.Instruction -> m [AST.Named AST.Instruction]
getElemPtrInstr acc instr@(varName AST.:= getElemPtr@AST.GetElementPtr {..}) = do
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

updateIntMap :: MonadState StateMap m => G.Global -> m ()
updateIntMap f = do
  forM_ (G.basicBlocks f) iterBB
    where
      iterBB bb@(G.BasicBlock name instrs term) = mapM updateVar instrs
      updateVar instr@(varName AST.:= call@AST.Call {}) =
        case getMemAlloc call of
          Right size -> modify $ \sm -> sm { intMap =  M.insert varName size (intMap sm)}
          _ -> pure ()
      updateVar instr@(AST.Do AST.Store {..}) = do
        memAllocPtrs <- gets intMap
        case (getOperandName address, getOperandName value) of
          (Just addrName, Just operandName) -> do
            case M.lookup operandName memAllocPtrs of
              Just size -> modify $ \sm -> sm { intMap =  M.insert addrName size (intMap sm)}
              Nothing -> pure ()
          _ -> pure ()
      updateVar instr@(varName AST.:= AST.Load {..}) = do
        memAllocPtrs <- gets intMap
        case getOperandName address of
          Just addrName -> do
            case M.lookup addrName memAllocPtrs of
              Just size -> modify $ \sm -> sm { intMap =  M.insert varName size (intMap sm)}
              Nothing -> pure ()
          _ -> pure ()
      updateVar instr@(varName AST.:= AST.BitCast {..}) = do
        case getOperandName operand0 of
          Just operandName -> addToTracker varName operandName
          _ -> pure ()
      updateVar _ = pure ()

      addToTracker varName operandName = do
        memAllocPtrs <- gets intMap
        case M.lookup operandName memAllocPtrs of
          Just memSize -> modify $ \sm -> sm { intMap =  M.insert varName memSize (intMap sm) }
          _ -> pure ()

getMemAlloc :: AST.Instruction -> Either String Integer
getMemAlloc (AST.Call {..}) =
  case function of
    Right (AST.ConstantOperand (C.GlobalReference (AST.Name funcName))) ->
      if funcName == "malloc"
         then (Right . constantToInt . fst . head) arguments
         else Left "Error: Expected one argument"
    Left _ -> Left "Error: not a malloc"


instrument :: (MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) => G.Global -> m AST.Definition
instrument f = do
  let bbs = G.basicBlocks f
  if length bbs == 0
     then pure $ AST.GlobalDefinition f
     else do
       newBBs <- mapM filterDbgInstr bbs
       instrumented <- mapM addBoundAssert newBBs
       pure $ AST.GlobalDefinition $ f { G.basicBlocks = instrumented }
  where
    filterDbgInstr bb@(G.BasicBlock name instrs term) = do
      newInstrs <- filterM isDbgInstr instrs
      pure $ G.BasicBlock name newInstrs term

    addBoundAssert bb@(G.BasicBlock name instrs term) = do
      newInstrs <- foldM getElemPtrInstr [] instrs
      pure $ G.BasicBlock name newInstrs term


helloWorldPass :: HaskellPass ()
helloWorldPass mod = do
  let defs = AST.moduleDefinitions mod
  defBoundChecker
  mapM_ iterDef (reverse defs)
  addGlobalString >> outOfBoundErrLogFormat >> return ()
  where
    iterDef d@(AST.GlobalDefinition f@(G.Function {})) = do
      updateIntMap f
      instrumented <- instrument f
      LLVM.emitDefn instrumented
    iterDef d@(AST.MetadataNodeDefinition nodeId mdNode) = do
      modify $ \sm -> sm { debugMap = M.insert nodeId mdNode (debugMap sm) }
      LLVM.emitDefn d
    iterDef d = LLVM.emitDefn d

addGlobalString :: IRBuilderT Env ()
addGlobalString = do
  LLVM.globalStringPtr "this is instrumented string\n" (AST.Name "globalProverStr")
  pure ()

outOfBoundErrLogFormat :: IRBuilderT Env ()
outOfBoundErrLogFormat = do
  LLVM.globalStringPtr "Found out of bound access: [%s:%d:%d]: \n\t array size: %d, indexed by: %d \n \t variable name: %s, allocated at: %d\n" (AST.Name "OUT_OF_BOUND_LOG_FORMAT")
  pure ()

callProver :: AST.Named AST.Instruction
callProver =
  AST.Do
    AST.Call {
      tailCallKind = Nothing,
      AST.callingConvention = AST.C,
      AST.returnAttributes = [],
      AST.type' = voidRetTyp,
      AST.function = Right (AST.ConstantOperand (C.GlobalReference (AST.Name "prover"))),
      AST.arguments = [],
      AST.functionAttributes = [],
      AST.metadata = []
    }

callBoundAssert :: (MonadState StateMap m, MonadIRBuilder m, MonadModuleBuilder m) =>
  Integer -> Integer -> p -> [(a1, AST.MDRef a2)] -> m (AST.Named AST.Instruction)
callBoundAssert arrSize idx targetAddrName metadata = do
  (varName, allocatedFileName, allocatedLine) <- getVarSource targetAddrName
  (accessFileName, funcName, accessLine, accessCol) <- getMDScope metadata
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
        AST.function = Right (AST.ConstantOperand (C.GlobalReference (AST.Name "boundAssertion"))),
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

  LLVM.function "boundAssertion" [arrSize, idx, line, col, fileName, varName, allocatedLine] AST.void body
    where
      body ops@(arrSize' : idx' : line' : col' : fileName' : varName' : allocatedLine' : []) =  mdo
        outOfBoundAccess <- LLVM.icmp AST.SGT idx' arrSize'
        LLVM.condBr outOfBoundAccess panic ret
        panic <- LLVM.block `LLVM.named` "panic"
        do
          LLVM.call voidRetTyp libcPrintf [
            (AST.ConstantOperand (C.GlobalReference (AST.Name "OUT_OF_BOUND_LOG_FORMAT")), []),
            (AST.LocalReference AST.ptr (getLocalOperandName fileName'), []),
            (AST.LocalReference AST.ptr (getLocalOperandName line'), []),
            (AST.LocalReference AST.ptr (getLocalOperandName col'), []),
            (AST.LocalReference AST.ptr (getLocalOperandName arrSize'), []),
            (AST.LocalReference AST.ptr (getLocalOperandName idx'), []),
            (AST.LocalReference AST.ptr (getLocalOperandName varName'), []),
            (AST.LocalReference AST.ptr (getLocalOperandName allocatedLine'), [])]
          LLVM.call voidRetTyp libcExit [(makeInt32Operand 1, [])]
        ret <- LLVM.block `LLVM.named` "ret"
        pure ()

defProver :: AST.Definition
defProver = AST.GlobalDefinition G.functionDefaults
  { G.name = AST.Name "prover"
  , G.parameters = ([], False)
  , G.returnType = AST.void
  , G.basicBlocks = [body]
  }
  where
    ptrTyp = AST.PointerType (AST.AddrSpace 0)
    body = AST.BasicBlock
        (AST.UnName 1)
        [
          AST.UnName 1 AST.:= AST.Load {
            AST.volatile = False,
            AST.type' = AST.ptr,
            AST.address = AST.ConstantOperand (C.GlobalReference "globalProverStr"),
            AST.maybeAtomicity = Nothing,
            AST.alignment = 0,
            AST.metadata = []
                              },
          AST.Do
            AST.Call {
              tailCallKind = Nothing,
              AST.callingConvention = AST.C,
              AST.returnAttributes = [],
              AST.type' = AST.FunctionType AST.void [] False,
              AST.function = Right (AST.ConstantOperand (C.GlobalReference (AST.Name "printf"))),
              AST.arguments = [(AST.ConstantOperand (C.GlobalReference (AST.Name "globalProverStr")), [])],
              AST.functionAttributes = [],
              AST.metadata = []
            }
        ]
        (AST.Do $ AST.Ret Nothing [])

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
