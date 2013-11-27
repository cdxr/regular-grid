module Data.Graph.Grid.Cheby
(
    Cheby(..),
    module Data.Graph.Grid.Vertex,
)
where

import Data.Monoid
import Data.Ix
import Data.Hashable

import Data.Graph.Grid.Vertex


-- | A vertex with eight neighbors (chebyshev distance)
data Cheby = Cheby !Integer !Integer
    deriving (Show, Eq, Ord, Ix)

instance Monoid Cheby where
    mempty = Cheby 0 0
    Cheby x y `mappend` Cheby x' y' = Cheby (x+x') (y+y')

instance Hashable Cheby where
    hashWithSalt s (Cheby i j) = hashWithSalt s (i, j)

instance Vertex Cheby where
    distance (Cheby x y) (Cheby x' y') = max (abs $ x - x') (abs $ y - y')

    -- TODO: implement enumDistance

