{-# LANGUAGE Rank2Types #-}

module Control.Lens.SplitMorphism (
    reverseEpi
  , reverseMono
  , composeSplitEpiMono
  , composeSplitEpiPrism
  , composeSplitMonoEpi
  , epiAsWedge
  , monoAsWedge
  ) where

import Control.Lens
import Control.Lens.Format (Format (..))
import Control.Lens.SplitEpi (SplitEpi (..))
import Control.Lens.SplitMono (SplitMono (..))
import Control.Lens.Wedge (Wedge (..))

-- |  Swapping `get` and `reverseGet` yields a `SplitMono.
reverseEpi :: SplitEpi a b -> SplitMono b a
reverseEpi (SplitEpi x y) = SplitMono y x

-- |  Swapping `get` and `reverseGet` yields a `SplitEpi.
reverseMono :: SplitMono a b -> SplitEpi b a
reverseMono (SplitMono x y) = SplitEpi y x

-- | Composition between SplitMono and SplitEpi.
composeSplitMonoEpi :: SplitMono a b -> SplitEpi b c -> Wedge a c
composeSplitMonoEpi (SplitMono x y) (SplitEpi q w) =
    Wedge (q . x) (y . w)

-- | Composition between SplitEpi and SplitMono.
composeSplitEpiMono :: SplitEpi a b -> SplitMono b c -> Wedge a c
composeSplitEpiMono (SplitEpi x y) (SplitMono q w) =
    Wedge (q . x) (y . w)

-- | Composition between SplitEpi and Prism.
composeSplitEpiPrism :: SplitEpi a b -> Prism' b c -> Format a c
composeSplitEpiPrism (SplitEpi x y) p =
    Format ((^? p) . x) (y . review p)

-- | SplitEpi as Wedge.
epiAsWedge :: SplitEpi a b -> Wedge a b
epiAsWedge (SplitEpi x y) = Wedge x y

-- | SplitMono as Wedge.
monoAsWedge :: SplitMono a b -> Wedge a b
monoAsWedge (SplitMono x y) = Wedge x y

