module Main (
    main
  ) where

import qualified FormatSpec as F
import qualified SplitEpiSpec as SE

main :: IO ()
main = do
    F.checkProps
    SE.checkProps

