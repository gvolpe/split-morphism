{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Lens.Wedge where

import Control.Lens
import Data.Functor.Invariant.TH

{- | Composition of a `Control.Lens.SplitMono.SplitMono` and a `Control.Lens.SplitEpi.SplitEpi`, yielding an even weaker structure where neither
`reverseGet . get` and `get . reverseGet` is an identity but both are idempotent.
-}
data Wedge a b = Wedge
    { get :: a -> b
    , reverseGet :: b -> a
    }
$(deriveInvariant ''Wedge)

-- | Normalize `a` via a round-trip through `b`.
normalizeA :: Wedge a b -> a -> a
normalizeA w = reverseGet w . get w

-- | Normalize `b` via a round-trip through `a`.
normalizeB :: Wedge a b -> b -> b
normalizeB w = get w . reverseGet w

-- | Swapping `get` and `reverseGet` yields a Wedge.
reverse :: Wedge a b -> Wedge b a
reverse (Wedge f g) = Wedge g f

-- | Compose with another Wedge.
composeWedge :: Wedge a b -> Wedge b c -> Wedge a c
composeWedge (Wedge x y) (Wedge q w) =
    Wedge (q . x) (y . w)

-- | Compose with an Iso.
composeIso :: Wedge a b -> Iso' b c -> Wedge a c
composeIso (Wedge x y) i =
    Wedge ((^. i) . x) (y . review i)

-- | An Isomorphism is trivially a Wedge.
fromIso :: Iso' a b -> Wedge a b
fromIso i = Wedge (^. i) (review i)

