{-# LANGUAGE Rank2Types #-}

module Control.Lens.Wedge where

import Control.Lens

{- | Composition of a `SplitMono` and a `SplitEpi`, yielding an even weaker structure where neither
`reverseGet . get` and `get . reverseGet` is an identity but both are idempotent.
-}
data Wedge a b = Wedge
    { get :: a -> b
    , reverseGet :: b -> a
    }

reverse :: Wedge a b -> Wedge b a
reverse (Wedge f g) = Wedge g f

-- | An Isomorphism is trivially a Wedge.
fromIso :: Iso' a b -> Wedge a b
fromIso i = Wedge (f i) (g i)
  where
    f :: Iso' a b -> a -> b
    f p x = x ^. p
    g :: Iso' a b -> b -> a
    g = review

