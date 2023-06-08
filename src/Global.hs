module Global where

import qualified LLVM.AST as AST

libcPrintfNm :: AST.Name
libcPrintfNm = AST.mkName "printf"

libcExitNm :: AST.Name
libcExitNm = AST.mkName "exit"

outOfBoundAssertFmtNm :: String
outOfBoundAssertFmtNm = "OUT_OF_BOUND_ASSERT_FMT"

mapUninitAccessAssertFmtNm :: String
mapUninitAccessAssertFmtNm = "MAP_UNINIT_ACCESS_ASSERT_FMT"

ansiWhite :: String
ansiWhite = "LEC_ANSI_WHITE_g767akzwihq04k3frbvijvx2l5gdn0sk"

ansiRed :: String
ansiRed = "LEC_ANSI_RED_g767akzwihq04k3frbvijvx2l5gdn0sk"

reportVarNm :: AST.Name
reportVarNm = AST.mkName "VAR_NAME"

reportFileNm :: AST.Name
reportFileNm = AST.mkName "FILE_NAME"

oobCheckerFnNm :: AST.Name
oobCheckerFnNm = AST.mkName "LEC_boundAssertion"

mapUninitCheckerFnNm :: AST.Name
mapUninitCheckerFnNm = AST.mkName "LEC_uninitMapAccessChecker"
