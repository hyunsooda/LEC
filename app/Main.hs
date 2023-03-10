{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Type
import Backend
import Pass

import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Data.List (isSuffixOf, isPrefixOf)
import qualified Data.Map as M
import Control.Monad.State hiding (void)
import Control.Monad.RWS hiding (void)
import System.Console.ANSI

import qualified LLVM.AST as AST

import LLVM.Internal.Context as LLVM
import LLVM.Internal.Module as LLVM
import LLVM.IRBuilder.Monad as LLVM
import LLVM.IRBuilder.Module as LLVM

printErr :: String -> IO ()
printErr err =
  setSGR [SetColor Foreground Vivid Red] >> print err >> setSGR [Reset]

runPass :: (Pass a) -> PassInput -> AST.Module -> IO (AST.Module, a)
runPass pass input mod = do
  let haskellPassState = StateMap { intMap = M.empty, debugMap = M.empty, sourceMap = M.empty }
  let irBuilderState = LLVM.emptyIRBuilder
  let modBuilderState = LLVM.emptyModuleBuilder
  let (((result, _), output), defs) = LLVM.runModuleBuilder modBuilderState $ do
                              (\x -> evalRWST x input haskellPassState) $
                                runIRBuilderT irBuilderState $ pass mod

  mapM_ (\log -> if isPrefixOf "ERROR: " log then printErr log else putStrLn log) output
  return (mod { AST.moduleDefinitions = (defProver:defs) }, result)

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
      (mod', _) <- runPass outOfBoundChecker [] mod
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
      (mod', _) <- runPass outOfBoundChecker [] mod
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
