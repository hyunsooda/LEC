{-# LANGUAGE FlexibleContexts #-}
module Pass where

import Type
import Backend

import Control.Monad.State hiding (void)
import qualified Data.Map as M
import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as G
import qualified LLVM.IRBuilder.Module as LLVM

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
