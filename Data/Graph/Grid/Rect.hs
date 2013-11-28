module Data.Graph.Grid.Rect
(
    Rect(..),
    module Data.Graph.Grid.Vertex,
)
where

import Data.Monoid
import Data.Ix
import Data.Hashable

import Data.Graph.Grid.Vertex


-- | A vertex with four neighbors (rectilinear/manhattan distance)
data Rect = Rect !Integer !Integer
    deriving (Show, Eq, Ord, Ix)

instance Monoid Rect where
    mempty = Rect 0 0
    Rect x y `mappend` Rect x' y' = Rect (x+x') (y+y')

instance Hashable Rect where
    hashWithSalt s (Rect i j) = hashWithSalt s (i, j)

instance Vertex Rect where
    degree _ = 4

    distance (Rect x y) (Rect x' y') = abs (x - x') + abs (y - y')

    enumDistance 0 = [Rect 0 0]
    enumDistance n =
        [Rect i (n - abs i) | i <- reverse range]
        ++ init (drop 1 [Rect i (-n + abs i) | i <- range])
      where
        range = [-n..n]

