split-morphism
==============

[![CircleCI](https://circleci.com/gh/gvolpe/split-morphism/tree/master.svg?style=svg)](https://circleci.com/gh/gvolpe/split-morphism/tree/master)

Experimental package representing [Split Epimorphism](https://ncatlab.org/nlab/show/split+epimorphism)s and [Split Monomorphism](https://ncatlab.org/nlab/show/split+monomorphism)s as presented by [Rob Norris (@tpolecat)](https://github.com/tpolecat) at [Scala eXchange 2018](https://skillsmatter.com/skillscasts/11626-keynote-pushing-types-and-gazing-at-the-stars).

Further development (in Scala) can be found in the [Gemini Ocs3
repository](https://github.com/gemini-hlsw/ocs3/tree/v0.11.13/modules/core/shared/src/main/scala/gem/optics).

## Non-Injective Optics

Standard 2-way optics deal with **invertible** mappings. `Iso a b` says that `a` and `b` are equal, so round-trips in either direction are identities. `Prism a b` says that there is some *subset* of `a` that is equal to `b`.

If we loosen the requirement that types be the same size we get a different kind of mapping, where the large type is squeezed into the small type in one direction or the other. An example is `Int ⟺ ByteString` by the standard widening/narrowing conversions. Note that the round-trip starting at `ByteString` is an identity, but the round-up starting at `Int` is merely **idempotent**: the first round-trip "normalizes" an `Int` into `ByteString` range and thereafter the round-trip is an identity.

This [phenomenon](https://ncatlab.org/nlab/show/split+epimorphism) is a thing, called a **split monomorphism** or a **split epimorphism** depending on which side is bigger. Note that every `Iso` is trivially a split where the idempotent round-trip happens to be an identity.

When we compose a `SplitMono` and a `SplitEpi` end-to-end in either direction we end up with a situation where neither round-trip is necessarily an identity but both are idempotent. I'm calling this a `Wedge` for lack of a better idea. Splits are trivially wedges where one of the idempotent round-trips happens to be an identity.

A `Format` is a weaker `Prism` where a *subset* of `a` forms a split epi with `b`. Every `Prism` is a `Format` where the split epi happens to be an `Iso`; and every `SplitEpi` forms a `Prism` where the subset of `a` is `a` itself.


```
               Wedge a b
                 a ? b

                   │                  Format a b
          ┌────────┴────────┐       ∃ a ⊂ a | a > b
          │                 │
                                           │
    SplitMono a b      SplitEpi a b   ─────┤
        a < b             a > b            │

          │                 │         Prism a b
          └────────┬────────┘       ∃ a ⊂ a | a = b
                   │                       │
                                           │
                Iso a b   ─────────────────┘
                 a = b
```

Adapted from the [Scala
version](https://github.com/gemini-hlsw/ocs3/blob/v0.11.13/modules/core/shared/src/main/scala/gem/optics/README.md).

## Examples

It is recommended to have qualified import of the modules, otherwise you might have some issues..

### Split Epimorphism

```
ghci> import qualified Control.Lens.SplitEpi as SE
ghci> import Data.Maybe (fromMaybe)
ghci> import Text.Read (readMaybe)
ghci> let epi = SE.SplitEpi (fromMaybe 0 . readMaybe) show :: SE.SplitEpi String Integer
ghci> SE.reverseGet epi 123
"123"
ghci> SE.get epi "foo"
0
ghci> SE.get epi "87"
87
```

### Split Monomorphism

```
ghci> import qualified Control.Lens.SplitMono as SM
ghci> let mono = SM.SplitMono toInteger fromInteger :: SM.SplitMono Int Integer
ghci> SM.get mono 1234567890123456789
1234567890123456789
ghci> SM.reverseGet mono 1234567890123456789
1234567890123456789
ghci> SM.reverseGet mono 123456789012345678901234
-7269072992350064654
```

### Format

```
ghci> import qualified Control.Lens.Format as F
ghci> let format = F.Format (\n -> if n > 0 then Just (n `mod` 2 == 0) else Nothing) (\n -> if n then 2 else 1) :: F.Format Int Bool
ghci> F.getMaybe format 0
Nothing
ghci> F.getMaybe format 1
Just False
ghci> F.getMaybe format 2
Just True
ghci> F.getMaybe format 3
Just False
ghci> F.reverseGet format True
2
ghci> F.reverseGet format False
1
```

### Wedge

```
ghci> import qualified Control.Lens.SplitEpi as SE
ghci> import qualified Control.Lens.SplitMono as SM
ghci> import qualified Control.Lens.SplitMorphism as S
ghci> import qualified Control.Lens.Wedge as W
ghci> let epi = SE.SplitEpi fromInteger toInteger :: SE.SplitEpi Integer Int
ghci> let mono = SM.SplitMono toInteger fromInteger :: SM.SplitMono Int Integer
ghci> let wedge = epi `S.composeSplitEpiMono` mono :: Wedge Integer Integer
ghci> W.get wedge 123
123
ghci> W.reverseGet  wedge 123
123
ghci> W.get wedge 123456789123456789000
-5670419392510072312
ghci> W.reverseGet wedge 123456789123456789000
-5670419392510072312
ghci> W.normalizeB wedge 123
123
ghci> W.normalizeA wedge 123
123
```

### Invariant mapping

All the data types exposed by this library, namely `SplitEpi`, `SplitMono`, `Format` and `Wedge`, have instances of `InvariantFunctor`.

#### SplitEpi

```
ghci> import Data.Functor.Invariant
ghci> let epi' = invmap (+1) (+2) epi
ghci> Se.reverseGet epi' 123
"125"
ghci> SE.get epi "foo"
1
ghci> SE.get epi' "87"
88
```

#### Format

```
ghci> import Data.Functor.Invariant
ghci> let format' = invmap not not format
ghci> F.reverseGet format' True
1
ghci> F.reverseGet format' False
2
```

### Conversions from Prism and Iso

#### A `Prism` can be converted into a `Format`:

```
ghci> import Control.Lens
ghci> import qualified Control.Lens.Format as F
ghci> import GHC.Natural
ghci> :{
ghci> | nat :: Prism' Integer Natural
ghci> | nat = prism toInteger $ \ i ->
ghci> |    if i < 0
ghci> |    then Left i
ghci> |    else Right (fromInteger i)
ghci> | :}
ghci> let f = F.fromPrism nat :: Format Integer Natural
```

#### An `Iso` can be converted into a `Format`, `SplitEpi`, `SplitMono` or `Wedge`:

```
ghci> import Control.Lens
ghci> import qualified Control.Lens.SplitEpi as SE
ghci> import qualified Control.Lens.SplitMono as SM
ghci> let nonIso = non 5 :: Iso' (Maybe Int) Int
ghci> let epi = SE.fromIso nonIso :: SplitEpi (Maybe Int) Int
ghci> let mono = SM.fromIso nonIso :: SplitMono (Maybe Int) Int
```

