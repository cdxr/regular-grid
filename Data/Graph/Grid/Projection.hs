module Data.Graph.Grid.Projection where

import Data.Graph.Grid


-- | Class for types that have a canonical position on the Euclidian plane
class Project a where
    project :: a -> (Double, Double)
    queryPoint :: (Double, Double) -> a
    queryRect :: (Double, Double) -> (Double, Double) -> [a]

instance Project Rect where
    project (Rect x y) = (fromIntegral x, fromIntegral y)
    queryPoint (x, y) = Rect (round x) (round y)
    queryRect a b = range (queryPoint a, queryPoint b)

instance Project Cheby where
    project (Cheby x y) = (fromIntegral x, fromIntegral y)
    queryPoint (x, y) = Cheby (round x) (round y)
    queryRect a b = range (queryPoint a, queryPoint b)


newtype HexH = HexH { unHexH :: Hex }
    deriving (Show, Eq, Ord)

instance Project HexH where
    project = projectHorizontal . unHexH
