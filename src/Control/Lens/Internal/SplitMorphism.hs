module Control.Lens.Internal.SplitMorphism where

-- | It represents Split Epimorphisms and Split Monomorphisms.
data SplitMorphism a b = SplitMorphism
    { get :: a -> b
    , reverseGet :: b -> a
    }

-- | `reverseGet . get`, yielding a normalized formatted value. Subsequent get/reverseGet cycles are idempotent.
normalize :: SplitMorphism a b -> a -> a
normalize (SplitMorphism f g) = g . f

-- | Epi to Mono and viceversa by swapping `get` and `reverseGet`.
reverse :: SplitMorphism a b -> SplitMorphism b a
reverse (SplitMorphism x y) = SplitMorphism y x

