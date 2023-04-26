module Main where

import System.Exit
import System.Process (callCommand, readProcessWithExitCode)
import System.FilePath (takeBaseName, replaceExtension)
import Data.String.Conversions

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Pass

main :: IO ()
main = do
  callCommand "cd tests/inputs; ./compile.sh"
  gt <- goldenTests
  defaultMain gt

goldenTests :: IO TestTree
goldenTests = testGroup "C-OOB" <$> sequence [basic]

runFile :: FilePath -> IO String
runFile file = do
  analyze LL file outputFile False
  (errCode, stdout', _) <- readProcessWithExitCode "lli" [outputFile] ""
  assertEqual "Must exit with error code 1" (ExitFailure 1) errCode
  callCommand ("rm " ++ outputFile)
  pure stdout'
    where outputFile = "output.ll"

basic :: IO TestTree
basic = do
  cFiles <- findByExtension [".ll"] "tests/inputs"
  pure $ testGroup
    "basic"
    [ goldenVsString (takeBaseName file) outFile (cs <$> runFile file)
      | file <- cFiles, let outFile = replaceExtension file ".golden"
    ]
