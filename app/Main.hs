{-# LANGUAGE RecordWildCards #-}

module Main where

import Cli
import Type
import Backend
import Pass

import System.Exit (exitFailure)
import System.Environment (getArgs)
import Data.List (isSuffixOf)

main :: IO ()
main = parseCli >>= instrument
  where
    instrument ExecParams{..} = 
      if isSuffixOf ".bc" inputPath
        then analyzeBC inputPath outputPath
      else if isSuffixOf ".ll" inputPath
        then analyzeLL inputPath outputPath
      else do
        putStrLn ("Invalid file extension (need either .bc or .ll): " ++ inputPath)
        exitFailure
