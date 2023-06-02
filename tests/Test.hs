import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Test.Tasty.Hspec

import OOB
import CFGSpec
import MetadataSpec

import System.Process

compile :: IO ()
compile = callCommand "cd tests/inputs; ./compile.sh"

main :: IO ()
main = do
  compile

  oobBasicTest <- oobBasic
  cfgBasicTest <- cfgBasic
  cfgTermTest <- cfgTerm
  typeLookupTest <- typLookupBasic
  demangleFnNameTest <- demangleFnNameSpec

  defaultMain (testGroup "All tests" [
                  testGroup "OOB test" [oobBasicTest]
                , testGroup "CFG test" [cfgBasicTest, cfgTermTest]
                , testGroup "Metadata test" [typeLookupTest, demangleFnNameTest]
              ])
