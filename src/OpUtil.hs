{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module OpUtil where

import Type
import Metadata

import Control.Monad.State hiding (void)
import qualified Data.Map as M
import Data.Text.Lazy (unpack)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C

import Prettyprinter hiding (line, line', column)
import LLVM.Pretty
import Debug.Trace

panic :: (Monad m, Pretty a) => String -> a -> m b
panic errStr expr = do
  traceM $ unpack $ ppll expr
  error errStr

isNotDebugInstr :: AST.Named AST.Instruction -> Bool
isNotDebugInstr (AST.Do AST.Call{..}) =
  case getFuncName function of
    Just name ->
      if name == "llvm.dbg.declare" then False else True
    _ -> True
isNotDebugInstr _ = True

filterDbgInstr :: G.BasicBlock -> G.BasicBlock
filterDbgInstr (AST.BasicBlock name instrs term) =
  AST.BasicBlock name (filter isNotDebugInstr instrs) term

filterDebugInfo :: MonadState StateMap m => AST.Named AST.Instruction -> m Bool
filterDebugInfo instr@(AST.Do AST.Call{function = func, arguments}) = do
  if (not . isNotDebugInstr $ instr) then
    let (varMD, sourceMapMD) = (head arguments, arguments !! 1)
        varName = getVarName varMD
      in do
     varInfo <- getVarInfo sourceMapMD
     addVarInfo varName varInfo
     pure False
  else
    pure True
filterDebugInfo _ = pure True

typeSize :: AST.Type -> Either String Int
typeSize typ = case typ of
  AST.IntegerType {..} -> Right $ fromIntegral typeBits
  -- TODO: Consider multiple dimension
  AST.ArrayType   {..} -> typeSize elementType
  _ -> Left $ "Unimplemented type: " ++ show typ

constantToInt :: AST.Operand -> Integer
constantToInt (AST.ConstantOperand (C.Int {..})) = integerValue
constantToInt (AST.ConstantOperand (C.Null {})) = 0

getIntValue :: MonadState StateMap m => AST.Operand -> m Integer
getIntValue c@(AST.ConstantOperand _) = pure $ constantToInt c

getIntValue e@(AST.LocalReference _ name) = do
  memAllocPtrs <- gets intMap
  case M.lookup name memAllocPtrs of
    Just size -> pure size
    _ -> panic "ERR1" e


