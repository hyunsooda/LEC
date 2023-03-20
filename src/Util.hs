module Util where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

mtstr = QuasiQuoter {
          quoteExp = stringE
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = undefined
        }


