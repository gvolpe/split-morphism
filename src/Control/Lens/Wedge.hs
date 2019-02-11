{-# LANGUAGE Rank2Types #-}

module Control.Lens.Wedge where

import Control.Lens
import Control.Lens.Internal.Iso (isoGet, isoReverseGet)

{- | Composition of a `SplitMono` and a `SplitEpi`, yielding an even weaker structure where neither
`reverseGet . get` and `get . reverseGet` is an identity but both are idempotent.
-}
data Wedge a b = Wedge
    { get :: a -> b
    , reverseGet :: b -> a
    }

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
    Wedge (isoGet i . x) (y . isoReverseGet i)

-- | An Isomorphism is trivially a Wedge.
fromIso :: Iso' a b -> Wedge a b
fromIso i = Wedge (isoGet i) (isoReverseGet i)

