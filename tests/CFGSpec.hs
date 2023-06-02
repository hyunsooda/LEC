{-# LANGUAGE FlexibleContexts  #-}

module CFGSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

import Pass
import Type
import Backend
import CFG

import Control.Monad.State
import Data.Text.Lazy (unpack)
import Data.List (elemIndex)

import qualified LLVM.AST as AST
import qualified LLVM.AST.Name as AST
import LLVM.Pretty

cfgBasic :: IO TestTree
cfgBasic = do
  mod <- getModule LL "tests/inputs/CFG/simple-cfg.ll"
  let wInstrs = evalState (collectInstrs mod fnName) initState
  let state = execState (initCFG mod) initState
      firstInstr = evalState (cfgGetFirstInstr fnName) state
      brTerm = nextOne fnName firstInstr state 8
      nextInstrOfBrTerm = nextOne fnName brTerm state 1
      condBrTerm = nextOne fnName nextInstrOfBrTerm state 2
      nextInstrsOfCondBrTerm = evalState (cfgGetNextInstrs fnName condBrTerm) state

  pure $ testGroup "Basic (simple-cfg.ll)"
    [ testCase "First instruction" $
        toStr firstInstr @?= "%1 = alloca i32, align 4 "
    , testCase "Branch terminator" $
        toStr brTerm @?= "br label %4 ,!dbg !4"
    , testCase "Next instruction of the branch terminator" $
        toStr nextInstrOfBrTerm @?= "%5 = load   ptr %3 , align 4 ,!dbg !5"
    , testCase "Next instruction of the conditional branch terminator" $
        toStr condBrTerm @?= "br i1 %6, label %7, label %14 ,!dbg !7"
    , testCase "True branch of the conditional branch terminator" $
        toStr (head nextInstrsOfCondBrTerm) @?= "%8 = load   ptr %3 , align 4 ,!dbg !8"
    , testCase "False branch of the conditional branch terminator" $
        toStr (nextInstrsOfCondBrTerm !! 1) @?= "%15 = load   ptr %1 , align 4 ,!dbg !14"
    , testCase "# of instruction of the main function" $
        length wInstrs @?= 23
    ]
  where
    fnName = AST.mkName "main"

cfgTerm :: IO TestTree
cfgTerm = do
  mod <- getModule LL "tests/inputs/CFG/simple-map.ll"
  let wInstrs = evalState (collectInstrs mod fnName) initState
  let state = execState (initCFG mod) initState
      firstInstr = evalState (cfgGetFirstInstr fnName) state
      invokeTerm = nextOne fnName firstInstr state 8
      nextInstrsOfInvokeTerm = evalState (cfgGetNextInstrs fnName invokeTerm) state

  pure $ testGroup "Invoke test (simple-map.ll)"
    [ testCase "Invoke terminaotr" $
        toStr invokeTerm @?= "invoke ccc void  @_ZNSt15_Rb_tree_header8_M_resetEv(ptr noundef nonnull align 8 dereferenceable(40) %3)  to label %7 unwind label %8 ,!dbg !99"
    , testCase "Normal branch" $
        toStr (head nextInstrsOfInvokeTerm) @?= "ret void ,!dbg !100"
    , testCase "Exception branch" $
        toStr (nextInstrsOfInvokeTerm !! 1) @?= "%9 = landingpad {ptr, i32}  ,!dbg !99 catch ptr zeroinitializer"
    , testCase "# of instruction" $
        length wInstrs @?= 14
    ]
  where
    fnName = AST.mkName "_ZNSt15_Rb_tree_headerC2Ev"

initCFG :: AST.Module -> State StateMap ()
initCFG mod = mapM addCode defs >> pure ()
  where
    defs = AST.moduleDefinitions mod

nextOne :: AST.Name -> InstructionWrap -> StateMap -> Int -> InstructionWrap
nextOne _ wInstr state 0 = wInstr
nextOne fnName wInstr state n =
  let nextInstrs = evalState (cfgGetNextInstrs fnName wInstr) state
   in
  nextOne fnName (head nextInstrs) state (n-1)

cfgGetFirstInstr :: AST.Name -> State StateMap InstructionWrap
cfgGetFirstInstr fnName = do
  bbs <- getBasicBlocks fnName
  case bbs of
    Just bbs ->
      let firstBB = head bbs
          instrs = getInstrs firstBB
          firstInstr = iwrap . head $ instrs
       in
      pure firstInstr
    _ -> undefined

cfgGetNextInstrs ::
  AST.Name -> InstructionWrap -> State StateMap [InstructionWrap]
cfgGetNextInstrs fnName wInstr = getNextInstr fnName wInstr

collectInstrs :: AST.Module -> AST.Name -> State StateMap [InstructionWrap]
collectInstrs mod funcNm = do
  let defs = AST.moduleDefinitions mod
  mapM addCode defs
  bbs <- getBasicBlocks funcNm
  case bbs of
    Just bbs ->
      let firstBB = head bbs
          instrs = getInstrs firstBB
          firstInstr = iwrap . head $ instrs
       in
      getAllNextInstrs funcNm [firstInstr] []
    _ -> pure []
  where
    getAllNextInstrs funcNm wInstrs@(_:_) nextInstrs = do
      let candidates = filter (not . isContained nextInstrs) wInstrs
      nextWInstrs <- forM candidates $ getNextInstr funcNm
      getAllNextInstrs funcNm (concat nextWInstrs) (nextInstrs ++ candidates)
    getAllNextInstrs _ [] acc = pure acc

    isContained set target =
      case target `elemIndex` set of
        Just _ -> True
        _ -> False

toStr :: InstructionWrap -> String
toStr instr@(I _) = unpack . ppll . wrapi $ instr
toStr term@(T _) = unpack . ppll . wrapt $ term

printInstr :: InstructionWrap -> IO ()
printInstr instr@(I _) = print . toStr $ instr
printInstr term@(T _) = print . toStr $ term
