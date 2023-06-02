module Util where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.ByteString.Short (ShortByteString)

mtstr :: QuasiQuoter
mtstr = QuasiQuoter {
          quoteExp = stringE
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = undefined
        }

removePunc :: String -> String
removePunc xs = [ x | x <- xs, not (x `elem` ['"', '\\']) ]

toString :: ShortByteString -> String
toString = removePunc . show
