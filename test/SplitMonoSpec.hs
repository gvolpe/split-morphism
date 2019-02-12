module SplitMonoSpec (
    checkProps
  ) where

import Control.Lens.SplitMono
import Data.Maybe (fromMaybe)
import Test.QuickCheck
import Text.Read (readMaybe)

mono1 :: SplitMono Int Integer
mono1 = SplitMono toInteger fromInteger

mono2 :: SplitMono Integer String
mono2 = SplitMono show (fromMaybe 0 . readMaybe)

mono3 :: SplitMono Int String
mono3 = mono1 `composeSplitMono` mono2

prop_normalize :: Eq a => SplitMono a b -> b -> Bool
prop_normalize mono x =
    reverseGet mono (normalize mono x) == reverseGet mono x

prop_normalized_reverse_get_round_trip :: Eq b => SplitMono a b -> b -> Bool
prop_normalized_reverse_get_round_trip mono x =
    (get mono . reverseGet mono) x' == x'
  where
    x' = normalize mono x

prop_get_round_trip :: Eq a => SplitMono a b -> a -> Bool
prop_get_round_trip mono x =
    (reverseGet mono . get mono) x == x

checkProps :: IO ()
checkProps = do
    quickCheck (prop_normalize mono1)
    quickCheck (prop_normalize mono2)
    quickCheck (prop_normalize mono3)
    quickCheck (prop_normalized_reverse_get_round_trip mono1)
    quickCheck (prop_normalized_reverse_get_round_trip mono2)
    quickCheck (prop_normalized_reverse_get_round_trip mono3)
    quickCheck (prop_get_round_trip mono1)
    quickCheck (prop_get_round_trip mono2)
    quickCheck (prop_get_round_trip mono3)

