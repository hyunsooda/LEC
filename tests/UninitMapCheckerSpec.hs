module UninitMapCheckerSpec where

import GoldenTest
import Test.Tasty

uninitMapAccessCheckerBasic :: String -> String -> IO TestTree
uninitMapAccessCheckerBasic testName filePath = goldenTest testName filePath
