{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Control.Applicative
import Data.Monoid

import Data.Graph.Grid


main :: IO ()
main = $(defaultMainGenerator)


instance Arbitrary Rect where
    arbitrary = Rect <$> arbitrary <*> arbitrary

instance Arbitrary Cheby where
    arbitrary = Cheby <$> arbitrary <*> arbitrary

instance Arbitrary Hex where
    arbitrary = Hex <$> arbitrary <*> arbitrary


data Proxy a = Proxy


testDistance0 :: forall a. (Vertex a) => Proxy a -> Bool
testDistance0 _ = enumDistance 0 == ([mempty] :: [a])

testDistanceElems :: forall a. (Vertex a) => Proxy a -> Positive Integer -> Property
testDistanceElems _ (Positive i) =
    mapSize (const 20) $   -- test only up to a distance of 20
        all ((==) i . distance mempty) (enumDistance i :: [a])

testAdjacent :: (Vertex a) => a -> Bool
testAdjacent t = all (adjacent t) (neighbors t)


prop_rect_distance_0 = testDistance0     (Proxy :: Proxy Rect)
prop_rect_dist_elems = testDistanceElems (Proxy :: Proxy Rect)
prop_rect_adjacent   = testAdjacent      :: Rect -> Bool

prop_cheby_distance_0  = testDistance0     (Proxy :: Proxy Cheby)
prop_cheby_dist_elems  = testDistanceElems (Proxy :: Proxy Cheby)
prop_cheby_adjacent    = testAdjacent      :: Cheby -> Bool

prop_hex_distance_0  = testDistance0     (Proxy :: Proxy Hex)
prop_hex_dist_elems  = testDistanceElems (Proxy :: Proxy Hex)
prop_hex_adjacent    = testAdjacent      :: Hex -> Bool
