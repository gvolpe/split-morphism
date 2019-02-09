module Control.Lens.SplitMorphism (
    reverseEpi
  , reverseMono
  ) where

import Control.Lens.SplitEpi (SplitEpi (..))
import Control.Lens.SplitMono (SplitMono (..))

-- |  Swapping `get` and `reverseGet` yields a `SplitMono.
reverseEpi :: SplitEpi a b -> SplitMono b a
reverseEpi (SplitEpi x y) = SplitMono y x

-- |  Swapping `get` and `reverseGet` yields a `SplitEpi.
reverseMono :: SplitMono a b -> SplitEpi b a
reverseMono (SplitMono x y) = SplitEpi y x

