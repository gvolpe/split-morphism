{-# LANGUAGE Rank2Types #-}

module Control.Lens.Format where

import Control.Lens
import Control.Lens.Internal.Iso (isoGet, isoReverseGet)

{- | A normalizing optic, isomorphic to Prism but with different laws, specifically `getMaybe` needs not to
be injective; i.e., distinct inputs may have the same `getMaybe` result, which combined with a subsequent
`reverseGet` yields a normalized form for `a`. Composition with stronger optics (`Prism` and `Iso`) yields
another `Format`.
-}
data Format a b = Format
    { getMaybe :: a -> Maybe b
    , reverseGet :: b -> a
    }

-- | `getMaybe` and `reverseGet`, yielding a normalized formatted value. Subsequent getMaybe/reverseGet cycles are idempotent.
normalize :: Format a b -> a -> Maybe a
normalize (Format f g) x = g <$> f x

-- | Compose with an Iso.
composeIso :: Format a b -> Iso' b c -> Format a c
composeIso (Format x y) i =
    Format (fmap (isoGet i) . x) (y . isoReverseGet i)

-- | A Prism is trivially a Format.
fromPrism :: Prism' a b -> Format a b
fromPrism p = Format (f p) (g p)
  where
    f :: Prism' a b -> a -> Maybe b
    f p x = x ^? p
    g :: Prism' a b -> b -> a
    g = review

-- | An Isomorphism is trivially a Format.
fromIso :: Iso' a b -> Format a b
fromIso i = Format (f i) (isoReverseGet i)
  where
    f :: Iso' a b -> a -> Maybe b
    f p x = x ^? p

