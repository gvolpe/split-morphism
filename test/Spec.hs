module Main (
    main
  ) where

import qualified FormatSpec as F
import qualified SplitEpiSpec as SE
import qualified SplitMonoSpec as SM
import qualified WedgeSpec as W

main :: IO ()
main = do
    F.checkProps
    SE.checkProps
    SM.checkProps
    W.checkProps

