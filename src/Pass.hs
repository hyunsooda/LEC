{-# LANGUAGE FlexibleContexts #-}

module Pass where

import Prelude hiding (mod, log)

import Type
import CFG
import Backend
import Metadata

import System.Console.ANSI
import Control.Monad.State hiding (void)
import Control.Monad.RWS hiding (void, pass)
import qualified Data.Map as M
import Data.List (isPrefixOf)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as AST

import LLVM.Internal.Context as LLVM
import LLVM.Internal.Module as LLVM
import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM

import Control.Monad.Identity
import System.IO.Unsafe

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
  emitBoundAssertion funcNames
  emitMapUninitChecker
  pure ()
  where
    iterDef d@(AST.GlobalDefinition f@(AST.Function {..})) =
      when (not . null $ basicBlocks) $ do
        updateIntMap f                -- step1 (pre-analysis)
        instrumented <- instrument f  -- step2 (emitting final code)
        LLVM.emitDefn instrumented
    iterDef _ = pure ()

    -- emit everything except for the functions that have basic blocks
    emitBase d@(AST.GlobalDefinition f@(AST.Function {..})) =
      if null basicBlocks then LLVM.emitDefn d else pure () 
    emitBase d = LLVM.emitDefn d

    initDebugInfo d@(AST.MetadataNodeDefinition nodeId mdNode) =
      modify $ \sm -> sm { debugMap = M.insert nodeId mdNode (debugMap sm) }
    initDebugInfo _ = pure ()

    emitBoundAssertion funcNames = do
      _ <- defBoundChecker
      unless ("exit" `elem` funcNames) $ emitLibcExit >> pure ()

    emitMapUninitChecker = do
      countFns <- findMapCountFns 
      mapM_ defMapUninitChecker countFns
  
    getFuncNames acc (AST.GlobalDefinition (AST.Function {..})) = getName name : acc
    getFuncNames acc _ = acc

    -- print state map
    ppSM =
      when debug $ do
        im <- gets intMap
        tm <- gets taintMap
        tell ["integer trace: " ++ show im]
        tell ["taint trace: " ++ show tm]
