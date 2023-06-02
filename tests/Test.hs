import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Test.Tasty.Hspec

import OOB
import CFGSpec
import MetadataSpec

main :: IO ()
main = do
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
