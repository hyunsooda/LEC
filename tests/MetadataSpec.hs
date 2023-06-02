{-# LANGUAGE FlexibleContexts  #-}

module MetadataSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

import Type
import Util
import Metadata
import Pass
import CFG

import Control.Monad.State
import System.Demangle.Pure (demangle)
import Data.ByteString.Short (ShortByteString)
import Data.Text.Lazy (unpack)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M

import qualified LLVM.AST as AST
import qualified LLVM.AST.Name as AST
import LLVM.Pretty

typLookupBasic :: IO TestTree
typLookupBasic = do
  mod <- getModule LL "tests/inputs/metadata/str.ll"
  let state = execState (initMD mod) initState
      (retTyp, paramTyps) = evalState (mdGetFnTyps fnNm) state

  pure $ testGroup "Basic (str.ll)"
    [ testCase "Test function name" $
        demangled @?= "test123"
    , testCase "Return type" $
        toStr retTyp @?= "bool"
    , testCase "# of parameters" $
        length paramTyps @?= 2
    , testCase "1st parameter type" $
        toStr (head paramTyps) @?= "map<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >, float, std::less<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > >, std::allocator<std::pair<const std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >, float> > >"
    , testCase "2nd parameter type" $
        toStr (paramTyps !! 1) @?= "basic_string<char, std::char_traits<char>, std::allocator<char> >"
    ]

  where
    targetFnName = "_Z7test123RSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEfSt4lessIS5_ESaISt4pairIKS5_fEEES5_"
    fnNm = AST.mkName targetFnName
    demangled =
      let fullNm = removePunc . show . fromJust . demangle $ targetFnName
       in
      takeWhile ((/=) '(') fullNm
    toStr = removePunc . unpack . ppll

demangleFnNameSpec :: IO TestTree
demangleFnNameSpec = do
  mod1 <- getModule LL "tests/inputs/CFG/simple-map.ll"
  let state1 = execState (initMD mod1) initState
      fnTyp1 = evalState (getDemangledFnStr demangedFnName1) state1

  mod2 <- getModule LL "tests/inputs/metadata/str.ll"
  let state2 = execState (initMD mod2) initState
      fnTyp2 = evalState (getDemangledFnStr demangedFnName2) state2

  pure $ testGroup "Demangling test"
    [ testCase "Demangling function name test 1" $
        fromJust fnTyp1 @?= expectedFnName1
    , testCase "Demangling function name test 2" $
        fromJust fnTyp2 @?= expectedFnName2
    ]

  where
    demangedFnName1 = "std::map<int, int, std::less<int>, std::allocator<std::pair<int const, int> > >::count(int const&) const"
    expectedFnName1 = AST.mkName "_ZNKSt3mapIiiSt4lessIiESaISt4pairIKiiEEE5countERS3_"
    demangedFnName2 = "std::map<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >, float, std::less<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > >, std::allocator<std::pair<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const, float> > >::count(std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > const&) const"
    expectedFnName2 = AST.mkName "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEfSt4lessIS5_ESaISt4pairIKS5_fEEE5countERS9_"

initMD :: AST.Module -> State StateMap ()
initMD mod = do
  mapM addCode defs
  mapM addDemangledFnStr defs
  mapM_ initDebugInfo defs
  pure ()
  where
    defs = AST.moduleDefinitions mod
    initDebugInfo (AST.MetadataNodeDefinition nodeId mdNode) =
      modify $ \sm -> sm { debugMap = M.insert nodeId mdNode (debugMap sm) }
    initDebugInfo _ = pure ()

mdGetFnTyps :: AST.Name -> State StateMap (String, [String])
mdGetFnTyps fnName = do
  fn <- getFunc fnName
  getFnTyps $ fromJust fn
