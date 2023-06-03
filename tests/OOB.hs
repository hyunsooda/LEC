module OOB where

import GoldenTest
import Test.Tasty

oobBasic :: String -> String -> IO TestTree
oobBasic testName filePath = goldenTest testName filePath
