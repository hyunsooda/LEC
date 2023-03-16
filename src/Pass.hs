{-# LANGUAGE FlexibleContexts #-}

module Pass where

import Type
import Backend

import System.Console.ANSI
import Control.Monad (when, unless)
import Control.Monad.State hiding (void)
import Control.Monad.RWS hiding (void)
import qualified Data.Map as M
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

runPass :: (Pass a) -> PassInput -> AST.Module -> Bool -> IO (AST.Module, a)
runPass pass input mod debug = do
  let haskellPassState = StateMap { intMap = M.empty, debugMap = M.empty, sourceMap = M.empty, taintMap = M.empty }
  let irBuilderState = LLVM.emptyIRBuilder
  let modBuilderState = LLVM.emptyModuleBuilder
  let (((result, _), output), defs) = LLVM.runModuleBuilder modBuilderState $ do
                              (\x -> evalRWST x input haskellPassState) $
                                runIRBuilderT irBuilderState $ pass mod debug

  mapM_ (\log -> if isPrefixOf "ERROR: " log then printErr log else putStrLn log) output
  return (mod { AST.moduleDefinitions = (defProver:defs) }, result)

analyzeLL :: FilePath -> String -> Bool -> IO ()
analyzeLL file outputPath debug = do
  fcts <- readFile file
  mod <- LLVM.withContext (\ctx -> do
    LLVM.withModuleFromLLVMAssembly ctx fcts LLVM.moduleAST)
  (mod', _) <- runPass outOfBoundChecker [] mod debug
  LLVM.withContext $ \ctx ->
    LLVM.withModuleFromAST
    ctx
    mod'
    (\modl -> LLVM.writeLLVMAssemblyToFile (LLVM.File outputPath) modl)

analyzeBC :: FilePath -> String -> Bool -> IO ()
analyzeBC file outputPath debug = do
  mod <- LLVM.withContext (\ctx -> do
    LLVM.withModuleFromBitcode ctx (LLVM.File file) LLVM.moduleAST)
  (mod', _) <- runPass outOfBoundChecker [] mod debug
  LLVM.withContext $ \ctx ->
    LLVM.withModuleFromAST
    ctx
    mod'
    (\modl -> LLVM.writeBitcodeToFile (LLVM.File outputPath) modl)

outOfBoundChecker :: AST.Module -> Bool -> IRBuilderT Env ()
outOfBoundChecker mod debug = do
  let defs = AST.moduleDefinitions mod
      funcNames = foldl getFuncNames [] defs
  emitChecker funcNames
  mapM_ iterDef (reverse defs)
  ppSM  
  addGlobalString >> outOfBoundErrLogFormat >> return ()
  where
    iterDef d@(AST.GlobalDefinition f@(G.Function {..})) = do
      updateIntMap f
      instrumented <- instrument f
      LLVM.emitDefn instrumented
    iterDef d@(AST.MetadataNodeDefinition nodeId mdNode) = do
      modify $ \sm -> sm { debugMap = M.insert nodeId mdNode (debugMap sm) }
      LLVM.emitDefn d
    iterDef d = LLVM.emitDefn d

    emitChecker funcNames = do
      defBoundChecker
      unless ("\"exit\"" `elem` funcNames) $ emitLibcExit >> pure ()
    
    getFuncNames acc d@(AST.GlobalDefinition f@(G.Function {..})) = getName name : acc
    getFuncNames acc _ = acc
    
    -- print state map
    ppSM =
      when debug $ do
        im <- gets intMap
        tm <- gets taintMap
        tell ["integer trace: " ++ show im]
        tell ["taint trace: " ++ show tm]
