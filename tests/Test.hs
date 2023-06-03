import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Test.Tasty.Hspec

import OOB
import CFGSpec
import MetadataSpec
import UninitMapCheckerSpec

import System.Process

-- Hide shell outputs
compile :: IO ()
compile = callCommand "cd tests/inputs; ./compile.sh 1> /dev/null 2> /dev/null"

main :: IO ()
main = do
  compile

  oobBasicTest <- oobBasic "basic" "tests/inputs/OOB"
  uninitMapAccessChekcerTest <- uninitMapAccessCheckerBasic "basic" "tests/inputs/uninit-map-access"
  cfgBasicTest <- cfgBasic
  cfgTermTest <- cfgTerm
  typeLookupTest <- typLookupBasic
  demangleFnNameTest <- demangleFnNameSpec

  defaultMain (testGroup "All tests" [
                  testGroup "OOB test" [oobBasicTest]
                , testGroup "Uninitialized map access test" [uninitMapAccessChekcerTest]
                , testGroup "CFG test" [cfgBasicTest, cfgTermTest]
                , testGroup "Metadata test" [typeLookupTest, demangleFnNameTest]
              ])
