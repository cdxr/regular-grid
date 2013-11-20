{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Test.QuickCheck

import Control.Applicative
import Data.Monoid

import Data.Tile


main = $(defaultMainGenerator)


instance Arbitrary Square where
    arbitrary = Square <$> arbitrary <*> arbitrary

instance Arbitrary Hex where
    arbitrary = Hex <$> arbitrary <*> arbitrary


data Proxy a = Proxy


testDistance0 :: forall a. (Tile a) => Proxy a -> Bool
testDistance0 _ = enumDistance 0 == ([mempty] :: [a])

testDistanceElems :: forall a. (Tile a) => Proxy a -> Positive Integer -> Property
testDistanceElems _ (Positive i) =
    mapSize (const 20) $   -- test only up to a distance of 20
        all ((==) i . distance mempty) (enumDistance i :: [a])

testAdjacent :: (Tile a) => a -> Bool
testAdjacent t = all (adjacent t) (neighbors t)


prop_square_distance_0 = testDistance0     (Proxy :: Proxy Square)
prop_square_dist_elems = testDistanceElems (Proxy :: Proxy Square)
prop_square_adjacent   = testAdjacent      :: Square -> Bool

prop_hex_distance_0    = testDistance0     (Proxy :: Proxy Hex)
prop_hex_dist_elems    = testDistanceElems (Proxy :: Proxy Hex)
prop_hex_adjacent      = testAdjacent      :: Hex -> Bool
