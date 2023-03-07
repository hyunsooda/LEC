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
import Data.List (isSuffixOf)
import Data.ByteString (readFile, writeFile, ByteString)
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
import qualified Data.Map as M
import System.Console.ANSI
import Data.List
import Debug.Trace
import qualified LLVM.AST.IntegerPredicate     as IP
import qualified LLVM.AST.Operand as OP
import Development.Placeholders

data StateMap = StateMap { intMap :: M.Map Name Integer
                         , debugMap :: M.Map MetadataNodeID MDNode
                         , sourceMap :: M.Map Name OP.DIVariable
                         } deriving Show

type HaskellPassInput = [String]
-- TODO: Remove me
-- type HaskellPassState = M.Map Name Integer
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


getOperandName (LocalReference _ varName) = Just varName
getOperandName (ConstantOperand (C.GlobalReference varName)) = Just varName
getOperandName _ = Nothing

getLocalOperandName (LocalReference _ varName) = varName

constantToInt :: IR.Operand -> Integer
constantToInt (ConstantOperand (C.Int {..})) = integerValue

makeInt32Operand :: Integer -> IR.Operand
makeInt32Operand n = ConstantOperand (C.Int 32 n)

-- TODO: Remove me
getFuncName :: IR.Instruction -> Maybe String
getFuncName (Call {..}) =
  case function of
    Right (ConstantOperand (C.GlobalReference (Name funcName))) -> Just . show $ funcName
    _ -> Nothing

-- TODO: Refactor me
getMetadataNodeId :: InstructionMetadata -> MetadataNodeID
getMetadataNodeId [(_, MDRef id)] = id

-- TODO: Refactor me
getMetadataNodeId2 (MDRef id) = id

-- TODO: Refactor me
getMD mdRef = do
  md <- gets debugMap
  let mdID = getMetadataNodeId2 mdRef
   in
   pure $ (M.!) md mdID

getVarNamed :: (Operand, a) -> Name
getVarNamed (MetadataOperand (MDValue (LocalReference _ varName)), _) = varName
getVarNamed _ = $notImplemented

getVarInfo (MetadataOperand (MDNode mdRef), _) = do
  md <- getMD mdRef
  pure $ diVar md
    where
      diVar (DINode var) = var

-- TODO: Add type
-- addVarInfo :: Name -> Op.DIVariable ->
addVarInfo name (OP.DIVariable var) = do
  modify $ \sm -> sm { sourceMap = M.insert name var (sourceMap sm) }

-- TODO: Add type and refactoring me
getMDFuncFileNames (DINode (OP.DIScope (OP.DILocalScope (OP.DISubprogram (OP.Subprogram {..}))))) = do
  fileName' <- fileName file
  pure (fileName', funcName)
    where
      funcName = show name
      fileName (Just mdRef) = do
        md' <- getMD mdRef
        pure $ getFileName md'
      fileName _ = pure "Failed to find file name"

getMDScope [(_, MDRef id)] = do
  md <- gets debugMap
  let (line, col, mdRef) = getMDLocation $ (M.!) md id
      mdScope = (M.!) md $ getMetadataNodeId2 mdRef
   in do
   (fileName, funcName) <- getMDFuncFileNames mdScope
   pure (fileName, funcName, fromIntegral line, fromIntegral col)

-- getMDLocation (DILocation loc) = loc
getMDLocation (DILocation (OP.Location {..})) = (line, column, scope)

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

getFileName (OP.DINode (OP.DIScope (OP.DIFile (OP.File {..})))) = show filename

getVarSource targetVarName = do
  sourceInfo <- gets sourceMap
  vals <- gets intMap
  let toBeFixed = fst . head $ M.toList vals
      -- source = (M.!) sourceInfo targetVarName in
      source = (M.!) sourceInfo toBeFixed in
      getSourceInfo source

  where
    getSourceInfo (OP.DILocalVariable (OP.LocalVariable {..})) = do
      fileName' <- fileName file
      pure $ (show name, fileName', fromIntegral line)
    fileName (Just mdRef) = do
      md <- getMD mdRef
      pure $ getFileName md

helloWorldPass :: HaskellPass ()
helloWorldPass mod = do
  let defs = IR.moduleDefinitions mod
  -- TODO: Remove me
  -- mapM_ visit defs
  defBoundChecker
  mapM_ visit (reverse defs)

  x <- gets intMap
  tell ["WWW : " ++ show x]

  addGlobalString >> outOfBoundErrLogFormat >> return ()
  where
    visit d@(IR.GlobalDefinition f@(Function {})) = do
      printBB f
      tell ["Hello from: " ++ (show $ G.name f)]
      tell ["  number of arguments: " ++ (show $ length $ fst $ G.parameters f)]
      -- LLVM.emitDefn d -- TODO: Remove me
      instrumented <- instrument f
      LLVM.emitDefn instrumented
    -- visit d@(IR.FunctionAttributes _ _) = pure ()
    visit d@(IR.MetadataNodeDefinition nodeId mdNode) = do
      modify $ \sm -> sm { debugMap = M.insert nodeId mdNode (debugMap sm) }
      LLVM.emitDefn d
    -- visit d@(IR.NamedMetadataDefinition _ _) = pure ()
    visit d = LLVM.emitDefn d



    printBB f = do
      forM (G.basicBlocks f) printInfo

    printInfo bb@(G.BasicBlock name instrs term) = do
      tell ["basic block name: " ++ (show name)]
      tell ["# of instructions: " ++ (show . length $ instrs)]
      mapM printInstr instrs
      -- TODO: Remove me
      -- x <- get
      -- tell ["X: " ++ show x]

    printInstr instr@(varName := call@Call {}) =
      case getMemAlloc call of
        -- Right size -> modify (M.insert varName size)
        Right size -> modify $ \sm -> sm { intMap =  M.insert varName size (intMap sm)}
        _ -> pure ()
    -- TODO: Remove me
    -- printInstr instr@(Do call@Call{}) = do
    --   tell ["QWE : " ++ (show (getFuncName call))]
    --   pure ()

    printInstr instr@(Do Store {..}) = do -- TODO: Refactoring me
      memAllocPtrs <- gets intMap
      case (getOperandName address, getOperandName value) of
        (Just addrName, Just operandName) -> do
          case M.lookup operandName memAllocPtrs of
            -- Just size -> modify (M.insert addrName size)
            Just size -> modify $ \sm -> sm { intMap =  M.insert addrName size (intMap sm)}
            Nothing -> pure ()
        _ -> pure ()
    printInstr instr@(varName := Load {..}) = do -- TODO: Refactoring me
      memAllocPtrs <- gets intMap
      case getOperandName address of
        Just addrName -> do
          case M.lookup addrName memAllocPtrs of
            -- Just size -> modify (M.insert varName size)
            Just size -> modify $ \sm -> sm { intMap =  M.insert varName size (intMap sm)}
            Nothing -> pure ()
        _ -> pure ()
    printInstr (varName := IR.GetElementPtr {..}) = do -- TODO: Refactoring me
      memAllocPtrs <- gets intMap
      md <- gets debugMap
      case getOperandName address of
        Just addrName -> do
          case M.lookup addrName memAllocPtrs of
            Just size ->
              let idx = constantToInt . head $ indices
                  mdID = getMetadataNodeId metadata in
                  case typeSize type' of
                    Right 32 ->
                      if idx > (size `div` 4)
                         then
                         -- TODO: Remove me
                         let a = 123
                             (line, col, mdRef) = getMDLocation $ (M.!) md mdID
                             ref = (M.!) md $ getMetadataNodeId2 mdRef
                          in
                            -- TODO: Remove me
                            -- aaa <- getMDScope metadata
                            -- tell ["ERROR: QWE " ++ show aaa]
                            -- pure ()
                            tell ["ERROR: " ++ show line ++ show col ++ show mdRef] >>
                            tell ["ERROR: " ++ show ref] >>
                            tell ["ERROR: Found ouf of bound access"]
                         else pure ()
                    _ -> pure ()

            Nothing -> pure ()
        _ -> pure ()
    printInstr instr = do
      trackingMemPtrs instr
      pure ()

    trackingMemPtrs (varName := instr) =
      case instr of
        IR.BitCast {..} ->
          case getOperandName operand0 of
            Just operandName -> addToTracker varName operandName
            _ -> pure ()
        -- TODO: Add more instruction types for unhandled cases
        _ -> pure ()
    trackingMemPtrs _ = pure ()

    addToTracker varName operandName = do
      memAllocPtrs <- gets intMap
      case M.lookup operandName memAllocPtrs of
        -- Just memSize -> modify (M.insert varName memSize)
        Just memSize -> modify $ \sm -> sm { intMap =  M.insert varName memSize (intMap sm) }
        _ -> pure ()

    getMemAlloc (Call {..}) =
      case function of
        Right (ConstantOperand (C.GlobalReference (Name funcName))) ->
          if funcName == "malloc"
             then (Right . constantToInt . fst . head) arguments
             else Left "Error: Expected one argument"
        Left _ -> Left "Error: not a malloc"

    instrument f = do
      let bbs = (G.basicBlocks f)
      if length bbs == 0
         then pure $ IR.GlobalDefinition f
         else do
           newBBs <- mapM filterDbgInstr bbs
           let instrumentedBB = addInstr $ head newBBs
               new = instrumentedBB:tail newBBs
            in do
              temp <- mapM addBoundAssert new
              pure $ IR.GlobalDefinition $ f { basicBlocks = temp }

    addInstr bb@(G.BasicBlock name instrs term) = G.BasicBlock name (callProver:instrs) term

    addBoundAssert bb@(G.BasicBlock name instrs term) = do
      newInstrs <- foldM getElemPtrInstr [] instrs
      pure $ G.BasicBlock name newInstrs term

    getElemPtrInstr acc instr@(varName := getElemPtr@IR.GetElementPtr {..}) = do
      memAllocPtrs <- gets intMap
      md <- gets debugMap
      case getOperandName address of
        Just addrName ->
          case M.lookup addrName memAllocPtrs of
            Just size ->
              let idx = constantToInt . head $ indices
                  mdID = getMetadataNodeId metadata
               in
               case typeSize type' of
                  Right 32 -> do
                    tell ["ZZZ2 : " ++ show addrName]
                    boundAssertCall <- callBoundAssert (size `div` 4) idx addrName metadata
                    pure $ acc ++ [instr, boundAssertCall]
                    -- pure $ acc ++ [instr, callBoundAssert (size `div` 4) idx metadata]
                  _ -> pure $ acc ++ [instr]
            Nothing -> pure $ acc ++ [instr]
        _ -> pure $ acc ++ [instr]
    getElemPtrInstr acc instr = pure $ acc ++ [instr]

    filterDbgInstr bb@(G.BasicBlock name instrs term) = do
      newInstrs <- filterM isDbgInstr instrs
      pure $ G.BasicBlock name newInstrs term
    isDbgInstr (Do call@Call{..}) =
      case getFuncName call of
        Just name ->
          if name == "\"llvm.dbg.declare\""
             then
             let (varMD, sourceMapMD) = (arguments !! 0, arguments !! 1)
                 varName = getVarNamed varMD
              in do
             varInfo <- getVarInfo sourceMapMD
             addVarInfo varName varInfo
             pure False
             else pure True
        _ -> pure True
    isDbgInstr _ = pure True

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

-- callBoundAssert :: Integer -> Integer -> InstructionMetadata -> Named Instruction
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

-- TODO: Remove me
callProver2 :: IRBuilderT Env IR.Operand
callProver2 =
  LLVM.call (FunctionType IR.void [] False) (ConstantOperand (C.GlobalReference (Name "prover"))) []


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
          -- LLVM.call voidRetTyp libcPrintf [(ConstantOperand (C.GlobalReference (Name "globalProverStr")), [])]
          -- printf <- L.externVarArgs (mkName "printf") [charStar] AST.i32
          -- arrSizeLoad <- LLVM.load IR.ptr arrSize' 4
          -- printfArgs <- LLVM.externVarArgs (mkName "printf") [IR.ptr, IR.i32] IR.i32
          LLVM.call voidRetTyp libcPrintf [
            (ConstantOperand (C.GlobalReference (Name "OUT_OF_BOUND_LOG_FORMAT")), []),
            (LocalReference IR.ptr (getLocalOperandName fileName'), []),
            (LocalReference IR.ptr (getLocalOperandName line'), []),
            (LocalReference IR.ptr (getLocalOperandName col'), []),
            (LocalReference IR.ptr (getLocalOperandName arrSize'), []),
            (LocalReference IR.ptr (getLocalOperandName idx'), []),
            (LocalReference IR.ptr (getLocalOperandName varName'), []),
            (LocalReference IR.ptr (getLocalOperandName allocatedLine'), [])]
          -- LLVM.call voidRetTyp libcPrintf [(LocalReference IR.ptr (getLocalOperandName arrSize'), [])]
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
              -- IR.arguments = [(IR.LocalReference IR.ptr (UnName 1), [])],
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
      -- bitcode <- LLVM.withContext $ (\ctx -> do
      --              LLVM.withModuleFromAST ctx mod' LLVM.moduleBitcode)
      -- writeFile outputPath bitcode
      -- exitSuccess
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
      -- TODO: Remove me
      (mod', _) <- runHaskellPass helloWorldPass [] mod
      -- assembly <- LLVM.withContext $ (\ctx -> do
      --               LLVM.withModuleFromAST ctx mod' LLVM.moduleLLVMAssembly)
      -- writeFile outputPath assembly
      -- exitSuccess

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
