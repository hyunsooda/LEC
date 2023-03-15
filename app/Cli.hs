module Cli where

import Options.Applicative as Opt

data ExecParams = ExecParams {
                               inputPath :: String
                             , outputPath :: String
                             } deriving Show

mkParams :: Opt.Parser ExecParams
mkParams =
  ExecParams
  <$> strOption (long "input" <> short 'i' <> help "input file path (.ll or .bc)")
  <*> strOption (long "output" <> short 'o' <> help "output file name")

opts =
  info (mkParams <**> helper) (fullDesc <> progDesc desc)
    where
      desc = title ++ "\n" ++ repo
      title = "LEC (LLVM-Based Exectend Compiler for Security Improvment)"
      repo = "https://github.com/hyunsooda/LEC"

parseCli = execParser opts