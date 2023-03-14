module Main where

import Type
import Backend
import Pass

import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Data.List (isSuffixOf)

main :: IO ()
main = do
  args <- getArgs
  if (length args) == 1
  then do
    let file = head args
    if isSuffixOf ".bc" file
      then analyzeBC file
    else if isSuffixOf ".ll" file
      then analyzeLL file 
    else do
      putStrLn ("Invalid file extension (need either .bc or .ll): " ++ file)
      exitFailure
  else do
    putStrLn $ "usage: HelloWorld FILE.{bc,ll}"
    exitFailure
      where
        outputPath = "output.ll"
