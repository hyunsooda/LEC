{-# LANGUAGE FlexibleContexts #-}

module CFG where

import Type
import Util

import System.Demangle.Pure (demangle)
import Data.List (elemIndex)
import qualified Data.Map as M
import Control.Monad.State hiding (void)
import Data.Maybe (fromJust)
import Data.ByteString.Short (ShortByteString)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C

data InstructionWrap =
  I (AST.Named AST.Instruction) | T (AST.Named AST.Terminator)
  deriving (Show, Eq)

getName :: AST.Name -> String
getName (AST.UnName a) = show a
getName (AST.Name a) = toString a

iwrap :: AST.Named AST.Instruction -> InstructionWrap
iwrap = I

twrap :: AST.Named AST.Terminator -> InstructionWrap
twrap = T

wrapi :: InstructionWrap -> AST.Named AST.Instruction
wrapi (I instr) = instr

wrapt :: InstructionWrap -> AST.Named AST.Terminator
wrapt (T term) = term

addCode :: MonadState StateMap m => AST.Definition -> m ()
addCode (AST.GlobalDefinition f@(G.Function {..})) = do
  cm <- gets codeMap
  modify $ \sm -> sm { codeMap = M.insert name f cm }
addCode _ = pure ()

getFunc :: MonadState StateMap m => AST.Name -> m (Maybe G.Global)
getFunc fnNm = do
  cm <- gets codeMap
  case M.lookup fnNm cm of
    Just fn@(G.Function {}) -> pure . Just $ fn
    _ -> pure Nothing

getBasicBlocks :: MonadState StateMap m => AST.Name -> m (Maybe [G.BasicBlock])
getBasicBlocks funcNm = do
  cm <- gets codeMap
  case M.lookup funcNm cm of
    Just def -> pure $ getBB def
    _ -> pure Nothing
  where
    getBB (G.Function {..}) = Just basicBlocks
    getBB _ = Nothing

getInstrs :: G.BasicBlock -> [AST.Named AST.Instruction]
getInstrs (G.BasicBlock _ instrs _) = instrs

getTerm :: G.BasicBlock -> AST.Named AST.Terminator
getTerm (G.BasicBlock _ _ term) = term

getNextInstr :: MonadState StateMap m =>
  AST.Name -> InstructionWrap -> m [InstructionWrap]
getNextInstr funcNm targetInstr = do
  cm <- gets codeMap
  case M.lookup funcNm cm of
    Just (G.Function {..}) ->
      let (nextInstrs, _) = foldl findInstr ([], basicBlocks) basicBlocks
       in
      pure nextInstrs
    _ -> pure []
  where
    findInstr (nextInstrs@(_:_), bbs) _ = (nextInstrs, bbs)
    findInstr ([], bbs) (G.BasicBlock bbName instrs term) =
      case targetInstr of
        I instr ->
          case instr `elemIndex` instrs of
            Just n -> if length instrs > n + 1
                         then ([I (instrs !! (n + 1))], bbs)
                         else ([T term], bbs)
            _ -> ([], bbs)
        T term' ->
          case term' of
            AST.Do AST.Ret {} -> ([], bbs)
            AST.Do AST.Unreachable {} -> ([], bbs)
            AST.Do AST.Br {..} -> handleSingleBr dest bbs
            AST.Do AST.CondBr {..} -> handleTwoBr trueDest falseDest bbs
            AST.Do AST.Invoke {..} -> handleTwoBr returnDest exceptionDest bbs
            _ AST.:= AST.Invoke {..} -> handleTwoBr returnDest exceptionDest bbs
            _ ->
              error "TODO(CFG.hs): unconsidered yet"
          --   -- AST.Switch {} -> undefined
          --   -- AST.IndirectBr {} -> undefined
          --   -- AST.Invoke {} -> undefined
          --   -- AST.Resume {} -> undefined
          --   -- AST.CleanupRet {} -> undefined
          --   -- AST.CatchRet {} -> undefined


    handleSingleBr bbName bbs =
      let bb = filter (findBB bbName) bbs in
      if null bb then ([], bbs)
                 else
                  case getFirstInstr $ head bb of
                    Just instr -> ([I instr], bbs)
                    _ -> ([T . getTerm $ head bb], bbs)

    handleTwoBr b1 b2 bbs =
      let (br1, _) = handleSingleBr b1 bbs
          (br2, _) = handleSingleBr b2 bbs
       in
      (br1 ++ br2, bbs)

    findBB targetBBName bb@(G.BasicBlock bbName _ _) = bbName == targetBBName
    getFirstInstr bb@(G.BasicBlock _ instr _) =
      if null instr then Nothing
                    else Just . head $ instr
