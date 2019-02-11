module FormatSpec (
    checkProps
  ) where

import Control.Lens.Format
import Test.QuickCheck

-- Our example Format injects ints into "positive and even"
example :: Format Int Bool
example =  Format
    { getMaybe = \n -> if n > 0 then Just (n `mod` 2 == 0) else Nothing
    , reverseGet = \b -> if b then 2 else 1
    }

prop_normalize :: Int -> Bool
prop_normalize x =
    (normalize example x >>= getMaybe example) == getMaybe example x

prop_parse_round_trip :: Int -> Bool
prop_parse_round_trip x =
    (reverseGet example <$> (oa >>= getMaybe example)) == oa
  where
    oa = normalize example x

prop_format_round_trip :: Bool -> Bool
prop_format_round_trip x =
    getMaybe example (reverseGet example x) == Just x

checkProps :: IO ()
checkProps = do
  quickCheck prop_normalize
  quickCheck prop_parse_round_trip
  quickCheck prop_format_round_trip

