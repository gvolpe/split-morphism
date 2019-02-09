module Control.Lens.Wedge where

{- | Composition of a `SplitMono` and a `SplitEpi`, yielding an even weaker structure where neither
`reverseGet . get` and `get . reverseGet` is an identity but both are idempotent.
-}
data Wedge a b = Wedge
    { get :: a -> b
    , reverseGet :: b -> a
    }

reverse :: Wedge a b -> Wedge b a
reverse (Wedge f g) = Wedge g f
