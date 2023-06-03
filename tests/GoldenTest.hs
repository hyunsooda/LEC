module GoldenTest where

import Pass

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import System.Exit
import System.Process (callCommand, readProcessWithExitCode)
import System.FilePath (takeBaseName, replaceExtension)
import Data.String.Conversions

runFile :: FilePath -> IO String
runFile file = do
  analyze LL file outputFile False
  (errCode, stdout', _) <- readProcessWithExitCode "lli" [outputFile] ""
  assertEqual "Must exit with error code 1" (ExitFailure 1) errCode
  callCommand ("rm " ++ outputFile)
  pure stdout'
    where outputFile = "output.ll"

goldenTest :: String -> String -> IO TestTree
goldenTest testName filePath = do
  files <- findByExtension [".ll"] filePath
  pure $ testGroup
    testName
    [ goldenVsString (takeBaseName file) outFile (cs <$> runFile file)
      | file <- files, let outFile = replaceExtension file ".golden"
    ]
