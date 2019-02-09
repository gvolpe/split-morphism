module Control.Lens.Wedge where

import Control.Lens.Internal.SplitMorphism

{- | Composition of a `SplitMono` and a `SplitEpi`, yielding an even weaker structure where neither
`reverseGet . get` and `get . reverseGet` is an identity but both are idempotent.
-}
type Wedge a b = SplitMorphism a b

