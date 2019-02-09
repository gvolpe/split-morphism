module Control.Lens.Format where

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

