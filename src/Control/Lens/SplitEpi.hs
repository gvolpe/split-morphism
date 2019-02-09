module Control.Lens.SplitEpi where

{- | A split epimorphism, which we can think of as a weaker `Iso a b` where `b` is a "smaller" type.
So `get . reverseGet` remains an identity but `reverseGet . get` is merely idempotent (i.e., it normalizes values in `a`).

The following statements hold:
  * `reverseGet` is a "section" of `get`,
  * `get` is a "retraction" of `reverseGet`,
  *  - `B` is a "retract" of `A`,
  *  - the pair `(get, reverseGet)` is a "splitting" of the idempotent `reverseGet . get`.
-}
data SplitEpi a b = SplitEpi
    { get :: a -> b
    , reverseGet :: b -> a
    }

-- | `reverseGet . get`, yielding a normalized formatted value. Subsequent get/reverseGet cycles are idempotent.
normalize :: SplitEpi a b -> a -> a
normalize (SplitEpi f g) = g . f

