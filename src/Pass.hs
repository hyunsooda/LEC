{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Pass where

import Prelude hiding (mod, log)

import Type
import CFG
import Backend
import Metadata

import System.Demangle.Pure (demangle)
import System.Console.ANSI
import Control.Monad.State hiding (void)
import Control.Monad.RWS hiding (void, pass)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.List (isPrefixOf, isInfixOf, findIndex)

import qualified LLVM.AST as AST hiding (callingConvention, returnAttributes, functionAttributes, alignment, metadata)
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Instruction as AST
import qualified LLVM.AST.CallingConvention as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.AST.Global as G

import LLVM.Internal.Context as LLVM
import LLVM.Internal.Module as LLVM
import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM
import LLVM.IRBuilder.Instruction as LLVM

import Control.Monad.Identity
import System.IO.Unsafe

import Debug.Trace

instance MonadIO Identity where
  liftIO = Identity . unsafePerformIO

data FileTyp = BC | LL deriving Eq

printErr :: String -> IO ()
printErr err =
  setSGR [SetColor Foreground Vivid Red] >> print err >> setSGR [Reset]

initState :: PassState
initState = StateMap { intMap = M.empty
                     , debugMap = M.empty
                     , sourceMap = M.empty
                     , taintMap = M.empty
                     , codeMap = M.empty
                     , demangledFuncMap = M.empty
                     }

runPass :: Pass a -> PassInput -> AST.Module -> Bool -> IO (AST.Module, a)
runPass pass input mod debug = do
  let irBuilderState = LLVM.emptyIRBuilder
  let modBuilderState = LLVM.emptyModuleBuilder
  let (((result, _), output), defs) = LLVM.runModuleBuilder modBuilderState $ do
                              (\x -> evalRWST x input initState) $
                                runIRBuilderT irBuilderState $ pass mod debug

  mapM_ (\log -> if "ERROR: " `isPrefixOf` log then printErr log else putStrLn log) output
  return (mod { AST.moduleDefinitions = defs }, result)

analyze :: FileTyp -> FilePath -> String -> Bool -> IO ()
analyze typ file outputPath debug = do
  mod <- getModule typ file
  (mod', _) <- runPass outOfBoundChecker [] mod debug
  LLVM.withContext $ \ctx ->
    LLVM.withModuleFromAST ctx mod' $ mod2File typ
  where
    mod2File LL modl = LLVM.writeLLVMAssemblyToFile (LLVM.File outputPath) modl
    mod2File BC modl = LLVM.writeBitcodeToFile (LLVM.File outputPath) modl

getModule :: FileTyp -> FilePath -> IO AST.Module
getModule fileTyp filePath = LLVM.withContext $ withModule fileTyp filePath
  where
    withModule LL file ctx = do
      fcts <- readFile file
      LLVM.withModuleFromLLVMAssembly ctx fcts LLVM.moduleAST
    withModule BC file ctx =
      LLVM.withModuleFromBitcode ctx (LLVM.File file) LLVM.moduleAST

outOfBoundChecker :: AST.Module -> Bool -> IRBuilderT Env ()
outOfBoundChecker mod debug = do
  let defs = AST.moduleDefinitions mod
      funcNames = foldl getFuncNames [] defs
  mapM addCode defs
  mapM addDemangledFnStr defs
  mapM_ initDebugInfo defs
  mapM_ emitBase defs
  mapM_ iterDef defs

  ppSM
  emitGlobalAnsiStr >> outOfBoundErrAssertFmt >> mapKeyExistAssertFmt
  emitLibC funcNames >> emitBoundAssertion >> emitMapUninitChecker

  pure ()
  where
    findBBIdxEMplaceHintUniq bbs = find bbs 0 Nothing
      where
        find ((AST.BasicBlock name instrs term) : residual) idx Nothing =
          case findIndex aux instrs of
            Just _ -> Just idx
            _ -> find residual (idx + 1) Nothing

        aux (_ AST.:= AST.Call {..}) =
          let fnNm = fromJust . getFuncName $ function
              demangled = fromJust . demangle $ fnNm
           in
          "_M_emplace_hint_unique" `isInfixOf` demangled
        aux _ = False

    replaceBB2Panic (AST.BasicBlock name _ _) =
      AST.BasicBlock name [reportCall, exitCall] term
      where
        reportCall = AST.Do AST.Call {
          AST.tailCallKind = Nothing
        , AST.callingConvention = AST.C
        , AST.returnAttributes = []
        , AST.type' = voidRetTyp
        , AST.function = Right libcPrintf
        , AST.arguments =
          [ (AST.ConstantOperand (AST.GlobalReference (AST.Name "MAP_UNINIT_ACCESS_ASSERT_FMT")), [])
          , (AST.ConstantOperand (AST.GlobalReference (AST.Name "LEC_ANSI_RED_g767akzwihq04k3frbvijvx2l5gdn0sk")), [])
          , (AST.LocalReference AST.ptr (AST.mkName "fileName"), [])
          , (AST.LocalReference AST.ptr (AST.mkName "line"), [])
          , (AST.LocalReference AST.ptr (AST.mkName "col"), [])
          , (AST.ConstantOperand (AST.GlobalReference (AST.Name "LEC_ANSI_WHITE_g767akzwihq04k3frbvijvx2l5gdn0sk")), [])
          , (AST.LocalReference AST.ptr (AST.mkName "varName"), [])
          , (AST.LocalReference AST.ptr (AST.mkName "keyName"), [])
          ]
        , AST.functionAttributes = []
        , AST.metadata = []
        }
        exitCall = AST.Do AST.Call {
          AST.tailCallKind = Nothing
        , AST.callingConvention = AST.C
        , AST.returnAttributes = []
        , AST.type' = voidRetTyp
        , AST.function = Right libcExit
        , AST.arguments = [(makeInt32Operand 1, [])]
        , AST.functionAttributes = []
        , AST.metadata = []
        }
        term = AST.Do AST.Ret {
          AST.returnOperand = Just $ AST.ConstantOperand (AST.Null AST.ptr)
        , AST.metadata' = []
        }

    iterDef (AST.GlobalDefinition f@(AST.Function {..})) = do
      when (not . null $ basicBlocks) $ do
        updateIntMap f                -- step1 (pre-analysis)
        instrumented <- instrument f  -- step2 (emitting final code)
        LLVM.emitDefn instrumented

    -- Add global variable debug information
    iterDef (AST.GlobalDefinition g@(AST.GlobalVariable {..})) = do
      if not . null $ metadata then do
        varInfo <- getGlobalVarInfo $ snd . head $ metadata
        addVarInfo name varInfo
      else
        pure ()

    iterDef _ = pure ()

    -- emit everything except for the functions that have basic blocks
    emitBase d@(AST.GlobalDefinition f@(AST.Function {..})) =
      if null basicBlocks then LLVM.emitDefn d else pure ()
    emitBase d = LLVM.emitDefn d

    initDebugInfo d@(AST.MetadataNodeDefinition nodeId mdNode) =
      modify $ \sm -> sm { debugMap = M.insert nodeId mdNode (debugMap sm) }
    initDebugInfo _ = pure ()

    emitLibC funcNames = do
      unless ("exit" `elem` funcNames) $ emitLibcExit >> pure ()
      unless ("printf" `elem` funcNames) $ emitLibcPrintf >> pure ()

    emitBoundAssertion = defBoundChecker

    emitMapUninitChecker = do
      accessFnStrs <- findMapAccessFnStrs
      mapM_ emitChecker accessFnStrs

      where
        emitChecker fnStr = do
          fn <- getFunc fnStr
          case fn of
            Just fn' -> do
              def <- defChecker fn'
              LLVM.emitDefn def
            Nothing -> pure ()

        defChecker f@(AST.Function { .. }) = do
          let bbIdx = fromJust . findBBIdxEMplaceHintUniq $ basicBlocks
              panicBB = replaceBB2Panic (basicBlocks !! bbIdx)
              bbs = map filterDbgInstr basicBlocks
              (bbs1, _:bbs2) = splitAt bbIdx bbs
              newBBs = bbs1 ++ [panicBB] ++ bbs2
              newFn = AST.GlobalDefinition $ f {
                G.name = AST.mkName "LEC_uninitMapAccessChecker"
              , G.parameters = ([
                  (fst parameters) !! 0
                , (fst parameters) !! 1
                , AST.Parameter AST.ptr (AST.mkName "line") []
                , AST.Parameter AST.ptr (AST.mkName "col") []
                , AST.Parameter AST.ptr (AST.mkName "fileName") []
                , AST.Parameter AST.ptr (AST.mkName "varName") []
                , AST.Parameter AST.ptr (AST.mkName "keyName") []
                ], False)
              , G.basicBlocks = newBBs
              , G.metadata = []
              }
           in
           pure newFn

        filterDbgInstr (AST.BasicBlock name instrs term) =
          AST.BasicBlock name (filter isNotDebugInstr instrs) term

    getFuncNames acc (AST.GlobalDefinition (AST.Function {..})) = getName name : acc
    getFuncNames acc _ = acc

    -- print state map
    ppSM =
      when debug $ do
        im <- gets intMap
        tm <- gets taintMap
        tell ["integer trace: " ++ show im]
        tell ["taint trace: " ++ show tm]
