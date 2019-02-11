{-# LANGUAGE Rank2Types #-}

module Control.Lens.Internal.Iso (
    isoGet
  , isoReverseGet
  ) where

import Control.Lens

isoGet :: Iso' a b -> a -> b
isoGet i x = x ^. i

isoReverseGet :: Iso' a b -> b -> a
isoReverseGet = review

