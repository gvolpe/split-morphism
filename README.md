split-morphism
==============

[![CircleCI](https://circleci.com/gh/gvolpe/split-morphism/tree/master.svg?style=svg)](https://circleci.com/gh/gvolpe/split-morphism/tree/master)

Experimental package representing [Split Epimorphism](https://ncatlab.org/nlab/show/split+epimorphism)s and [Split Monomorphism](https://ncatlab.org/nlab/show/split+monomorphism)s as presented by [Rob Norris (@tpolecat)](https://github.com/tpolecat) at [Scala eXchange 2018](https://skillsmatter.com/skillscasts/11626-keynote-pushing-types-and-gazing-at-the-stars).

Further developement (in Scala) can be found in the [Gemini Ocs3
repository](https://github.com/gemini-hlsw/ocs3/tree/develop/modules/core/shared/src/main/scala/gem/optics).

## Non-Injective Optics

Standard 2-way optics deal with **invertible** mappings. `Iso a b` says that `a` and `b` are equal, so round-trips in either direction are identities. `Prism a b` says that there is some *subset* of `a` that is equal to `b`.

If we loosen the requirement that types be the same size we get a different kind of mapping, where the large type is squeezed into the small type in one direction or the other. An example is `Int ⟺ ByteString` by the standard widening/narrowing conversions. Note that the round-trip starting at `ByteString` is an identity, but the round-up starting at `Int` is merely **idempotent**: the first round-trip "normalizes" an `Int` into `ByteString` range and thereafter the round-trip is an identity.

This [phenomenon](https://ncatlab.org/nlab/show/split+epimorphism) is a thing, called a **split monomorphism** or a **split epimorphism** depending on which side is bigger. Note that every `Iso` is trivially a split where the idempotent round-trip happens to be an identity.

When we compose a split mono and a split epi end-to-end in either direction we end up with a situation where neither round-trip is necessarily an identity but both are idempotent. I'm calling this a `Wedge` for lack of a better idea. Splits are trivially wedges where one of the idempotent round-trips happens to be an identity.

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
version](https://github.com/gemini-hlsw/ocs3/blob/develop/modules/core/shared/src/main/scala/gem/optics/README.md).

