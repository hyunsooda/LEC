module Main where

import Cli
import Pass

import System.Exit (exitFailure)
import Data.List (isSuffixOf)

main :: IO ()
main = parseCli >>= instrument
  where
    instrument ExecParams {..}
      | ".bc" `isSuffixOf` inputPath = analyze BC inputPath outputPath debug
      | ".ll" `isSuffixOf` inputPath = analyze LL inputPath outputPath debug
      | otherwise = do
        putStrLn ("Invalid file extension (need either .bc or .ll): " ++ inputPath)
        exitFailure
