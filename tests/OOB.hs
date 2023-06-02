module OOB where

import System.Exit
import System.Process (callCommand, readProcessWithExitCode)
import System.FilePath (takeBaseName, replaceExtension)
import Data.String.Conversions

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import Pass

runFile :: FilePath -> IO String
runFile file = do
  analyze LL file outputFile False
  (errCode, stdout', _) <- readProcessWithExitCode "lli" [outputFile] ""
  assertEqual "Must exit with error code 1" (ExitFailure 1) errCode
  callCommand ("rm " ++ outputFile)
  pure stdout'
    where outputFile = "output.ll"

oobBasic :: IO TestTree
oobBasic = do
  cFiles <- findByExtension [".ll"] "tests/inputs/OOB"
  pure $ testGroup
    "basic"
    [ goldenVsString (takeBaseName file) outFile (cs <$> runFile file)
      | file <- cFiles, let outFile = replaceExtension file ".golden"
    ]
