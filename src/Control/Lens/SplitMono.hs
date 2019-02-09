module Control.Lens.SplitMono where

{- | A split monomorphism, which we can think of as a weaker `Iso a b` where `a` is a "smaller" type.
So `reverseGet . get` remains an identity but `get . reverseGet` is merely idempotent (i.e., it normalizes values in `b`).

The following statements hold:
  * `reverseGet` is a "retraction" of `get`,
  * `get` is a "section" of `reverseGet`,
  * `a` is a "retract" of `b`,
  * the pair `(reverseGet, get)` is a "splitting" of the idempotent `get . reverseGet`.
-}
data SplitMono a b = SplitMono
    { get :: a -> b
    , reverseGet :: b -> a
    }

-- | `reverseGet . get`, yielding a normalized formatted value. Subsequent get/reverseGet cycles are idempotent.
normalize :: SplitMono a b -> a -> a
normalize (SplitMono f g) = g . f

