module Control.Lens.SplitMono where

import Control.Lens.Internal.SplitMorphism

{- | A split monomorphism, which we can think of as a weaker `Iso a b` where `a` is a "smaller" type.
So `reverseGet . get` remains an identity but `get . reverseGet` is merely idempotent (i.e., it normalizes values in `b`).

The following statements hold:
  * `reverseGet` is a "retraction" of `get`,
  * `get` is a "section" of `reverseGet`,
  * `a` is a "retract" of `b`,
  * the pair `(reverseGet, get)` is a "splitting" of the idempotent `get . reverseGet`.
-}
type SplitMono a b = SplitMorphism a b

