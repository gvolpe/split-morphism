module SplitEpiSpec (
    checkProps
  ) where

import Control.Lens.SplitEpi
import Data.Maybe (fromMaybe)
import Test.QuickCheck
import Text.Read (readMaybe)

epi1 :: SplitEpi String Integer
epi1 = SplitEpi (fromMaybe 0 . readMaybe) show

epi2 :: SplitEpi Integer Int
epi2 = SplitEpi fromInteger toInteger

epi3 :: SplitEpi String Int
epi3 = epi1 `composeSplitEpi` epi2

prop_normalize :: Eq b => SplitEpi a b -> a -> Bool
prop_normalize epi x =
    get epi (normalize epi x) == get epi x

prop_normalized_get_round_trip :: Eq a => SplitEpi a b -> a -> Bool
prop_normalized_get_round_trip epi x =
    (reverseGet epi . get epi) x' == x'
  where
    x' = normalize epi x

prop_reverse_get_round_trip :: Eq b => SplitEpi a b -> b -> Bool
prop_reverse_get_round_trip epi x =
    (get epi . reverseGet epi) x == x

checkProps :: IO ()
checkProps = do
    quickCheck (prop_normalize epi1)
    quickCheck (prop_normalize epi2)
    quickCheck (prop_normalize epi3)
    quickCheck (prop_normalized_get_round_trip epi1)
    quickCheck (prop_normalized_get_round_trip epi2)
    quickCheck (prop_normalized_get_round_trip epi3)
    quickCheck (prop_reverse_get_round_trip epi1)
    quickCheck (prop_reverse_get_round_trip epi2)
    quickCheck (prop_reverse_get_round_trip epi3)

