{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Lens.Format where

import Control.Monad ((>=>))
import Control.Lens
import Data.Functor.Invariant.TH

{- | A normalizing optic, isomorphic to Prism but with different laws, specifically `getMaybe` needs not to
be injective; i.e., distinct inputs may have the same `getMaybe` result, which combined with a subsequent
`reverseGet` yields a normalized form for `a`. Composition with stronger optics (`Prism` and `Iso`) yields
another `Format`.
-}
data Format a b = Format
    { getMaybe :: a -> Maybe b
    , reverseGet :: b -> a
    }
$(deriveInvariant ''Format)

-- | `getMaybe` and `reverseGet`, yielding a normalized formatted value. Subsequent getMaybe/reverseGet cycles are idempotent.
normalize :: Format a b -> a -> Maybe a
normalize (Format f g) x = g <$> f x

-- | Compose with a Prism.
composePrism :: Format a b -> Prism' b c -> Format a c
composePrism (Format x y) p =
    Format (x >=> (^? p)) (y . review p)

-- | Compose with an Iso.
composeIso :: Format a b -> Iso' b c -> Format a c
composeIso (Format x y) i =
    Format (fmap (^. i) . x) (y . review i)

-- | A Prism is trivially a Format.
fromPrism :: Prism' a b -> Format a b
fromPrism p = Format (^? p) (review p)

-- | An Isomorphism is trivially a Format.
fromIso :: Iso' a b -> Format a b
fromIso i = Format (^? i) (review i)

