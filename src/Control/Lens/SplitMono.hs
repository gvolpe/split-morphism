{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Lens.SplitMono where

import Control.Lens
import Data.Functor.Invariant.TH

{- | A split monomorphism, which we can think of as a weaker `Iso` a b where `a` is a "smaller" type.
So `reverseGet . get` remains an identity but `get . reverseGet` is merely idempotent (i.e., it normalizes values in `b`).

The following statements hold:
  - `reverseGet` is a "retraction" of `get`,
  - `get` is a "section" of `reverseGet`,
  - `a` is a "retract" of `b`,
  - the pair `(reverseGet, get)` is a "splitting" of the idempotent `get . reverseGet`.
-}
data SplitMono a b = SplitMono
    { get :: a -> b
    , reverseGet :: b -> a
    }
$(deriveInvariant ''SplitMono)

-- | `reverseGet . get`, yielding a normalized formatted value. Subsequent get/reverseGet cycles are idempotent.
normalize :: SplitMono a b -> b -> b
normalize (SplitMono f g) = f . g

-- | Compose with another SplitMono.
composeSplitMono :: SplitMono a b -> SplitMono b c -> SplitMono a c
composeSplitMono (SplitMono x y) (SplitMono q w) =
    SplitMono (q . x) (y . w)

-- | Compose with an Iso.
composeIso :: SplitMono a b -> Iso' b c -> SplitMono a c
composeIso (SplitMono x y) i =
    SplitMono ((^. i) . x) (y . review i)

-- | An Isomorphism is trivially a SplitMono.
fromIso :: Iso' a b -> SplitMono a b
fromIso i = SplitMono (^. i) (review i)

