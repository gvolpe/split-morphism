module WedgeSpec (
    checkProps
  ) where

import qualified Control.Lens.SplitEpi as SE
import qualified Control.Lens.SplitMono as SM
import qualified Control.Lens.SplitMorphism as S
import Control.Lens.Wedge
import Test.QuickCheck

epi :: SE.SplitEpi Integer Int
epi = SE.SplitEpi fromInteger toInteger

mono :: SM.SplitMono Int Integer
mono = SM.SplitMono toInteger fromInteger

wedge :: Wedge Integer Integer
wedge = epi `S.composeSplitEpiMono` mono

prop_normalize_a :: Eq b => Wedge a b -> a -> Bool
prop_normalize_a w x =
    get w (normalizeA w x) == get w x

prop_normalize_b :: Eq a => Wedge a b -> b -> Bool
prop_normalize_b w x =
    reverseGet w (normalizeB w x) == reverseGet w x

prop_normalized_reverse_get_round_trip :: Eq b => Wedge a b -> b -> Bool
prop_normalized_reverse_get_round_trip w x =
    (get w . reverseGet w) x' == x'
  where
    x' = normalizeB w x

prop_normalized_get_round_trip :: Eq a => Wedge a b -> a -> Bool
prop_normalized_get_round_trip w x =
    (reverseGet w . get w) x' == x'
  where
    x' = normalizeA w x

checkProps :: IO ()
checkProps = do
    quickCheck (prop_normalize_a wedge)
    quickCheck (prop_normalize_b wedge)
    quickCheck (prop_normalized_reverse_get_round_trip wedge)
    quickCheck (prop_normalized_get_round_trip wedge)

