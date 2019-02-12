{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Lens.SplitEpi where

import Control.Lens
import Data.Functor.Invariant.TH

{- | A split epimorphism, which we can think of as a weaker `Iso` a b where `b` is a "smaller" type.
So `get . reverseGet` remains an identity but `reverseGet . get` is merely idempotent (i.e., it normalizes values in `a`).

The following statements hold:
  - `reverseGet` is a "section" of `get`,
  - `get` is a "retraction" of `reverseGet`,
  - `b` is a "retract" of `a`,
  - the pair `(get, reverseGet)` is a "splitting" of the idempotent `reverseGet . get`.
-}
data SplitEpi a b = SplitEpi
    { get :: a -> b
    , reverseGet :: b -> a
    }
$(deriveInvariant ''SplitEpi)

-- | `reverseGet . get`, yielding a normalized formatted value. Subsequent get/reverseGet cycles are idempotent.
normalize :: SplitEpi a b -> a -> a
normalize (SplitEpi f g) = g . f

-- | Compose with another SplitEpi.
composeSplitEpi :: SplitEpi a b -> SplitEpi b c -> SplitEpi a c
composeSplitEpi (SplitEpi x y) (SplitEpi q w) =
    SplitEpi (q . x) (y . w)

-- | Compose with an Iso.
composeIso :: SplitEpi a b -> Iso' b c -> SplitEpi a c
composeIso (SplitEpi x y) i =
    SplitEpi ((^. i) . x) (y . review i)

-- | An Isomorphism is trivially a SplitEpi.
fromIso :: Iso' a b -> SplitEpi a b
fromIso i = SplitEpi (^. i) (review i)

