module Main where

import Cli
import Pass

import System.Exit (exitFailure)
import Data.List (isSuffixOf)

main :: IO ()
main = parseCli >>= instrument
  where
    instrument ExecParams{..} = 
      if isSuffixOf ".bc" inputPath
        then analyzeBC inputPath outputPath debug
      else if isSuffixOf ".ll" inputPath
        then analyzeLL inputPath outputPath debug
      else do
        putStrLn ("Invalid file extension (need either .bc or .ll): " ++ inputPath)
        exitFailure
