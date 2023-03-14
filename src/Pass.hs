{-# LANGUAGE FlexibleContexts #-}
module Pass where

import Type
import Backend

import System.Console.ANSI
import Control.Monad.State hiding (void)
import qualified Data.Map as M
import Control.Monad.RWS hiding (void)
import Data.List (isPrefixOf)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as G
import qualified LLVM.IRBuilder.Module as LLVM

import LLVM.Internal.Context as LLVM
import LLVM.Internal.Module as LLVM
import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM

printErr :: String -> IO ()
printErr err =
  setSGR [SetColor Foreground Vivid Red] >> print err >> setSGR [Reset]

runPass :: (Pass a) -> PassInput -> AST.Module -> IO (AST.Module, a)
runPass pass input mod = do
  let haskellPassState = StateMap { intMap = M.empty, debugMap = M.empty, sourceMap = M.empty, taintMap = M.empty }
  let irBuilderState = LLVM.emptyIRBuilder
  let modBuilderState = LLVM.emptyModuleBuilder
  let (((result, _), output), defs) = LLVM.runModuleBuilder modBuilderState $ do
                              (\x -> evalRWST x input haskellPassState) $
                                runIRBuilderT irBuilderState $ pass mod

  mapM_ (\log -> if isPrefixOf "ERROR: " log then printErr log else putStrLn log) output
  return (mod { AST.moduleDefinitions = (defProver:defs) }, result)

analyzeLL :: FilePath -> IO ()
analyzeLL file = do
  fcts <- readFile file
  mod <- LLVM.withContext (\ctx -> do
    LLVM.withModuleFromLLVMAssembly ctx fcts LLVM.moduleAST)
  (mod', _) <- runPass outOfBoundChecker [] mod
  LLVM.withContext $ \ctx ->
    LLVM.withModuleFromAST
    ctx
    mod'
    (\modl -> LLVM.writeLLVMAssemblyToFile (LLVM.File "output.ll") modl)

analyzeBC :: FilePath -> IO ()
analyzeBC file = do
  mod <- LLVM.withContext (\ctx -> do
    LLVM.withModuleFromBitcode ctx (LLVM.File file) LLVM.moduleAST)
  (mod', _) <- runPass outOfBoundChecker [] mod
  LLVM.withContext $ \ctx ->
    LLVM.withModuleFromAST
    ctx
    mod'
    (\modl -> LLVM.writeBitcodeToFile (LLVM.File "HELLO.bc") modl)

outOfBoundChecker :: Pass ()
outOfBoundChecker mod = do
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
