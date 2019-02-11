{-# LANGUAGE Rank2Types #-}

module Control.Lens.Internal.Prism (
    prismGet
  , prismReverseGet
  ) where

import Control.Lens

prismGet :: Prism' a b -> a -> Maybe b
prismGet p x = x ^? p

prismReverseGet :: Prism' a b -> b -> a
prismReverseGet = review

