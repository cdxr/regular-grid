module Data.Graph.Grid.Hex
(
    Hex(..),
    projectHorizontal,
    projectVertical,
    module Data.Graph.Grid.Vertex,
)
where

import Data.Monoid
import Data.Ix
import Data.Hashable

import Data.Graph.Grid.Vertex


-- | A vertex with six neighbors
data Hex = Hex !Integer !Integer
    deriving (Show, Eq, Ord, Ix)

instance Monoid Hex where
    mempty = Hex 0 0
    Hex x y `mappend` Hex x' y' = Hex (x+x') (y+y')

instance Hashable Hex where
    hashWithSalt s (Hex i j) = hashWithSalt s (i, j)
    
instance Vertex Hex where
    degree _ = 6

    distance (Hex x y) (Hex x' y') = maximum $ map abs [dy, dx, dy - dx]
      where
        dx = x - x'
        dy = y - y'

    enumDistance n
      | n < 0     = []
      | n == 0    = [start]
      | otherwise = walk relativePath start
      where
        walk path step =
            step : case path of
                [] -> []
                x:xs -> walk xs $ step <> x

        start = Hex n 0
        relativePath = init $ concatMap (replicate $ fromIntegral n)
            [ Hex (-1) (-1)
            , Hex (-1)   0
            , Hex   0    1
            , Hex   1    1
            , Hex   1    0
            , Hex   0  (-1)
            ]


-- | Determine the position of the Hex in a horizontal tesselation on a plane
projectHorizontal :: Hex -> (Double, Double)
projectHorizontal (Hex i j) = (- sqrt 3 * (y/2 - x), y * 3 / 2)
  where
    x = fromIntegral i
    y = fromIntegral j

-- | Determine the position of the Hex in a vertical tesselation on a plane
projectVertical :: Hex -> (Double, Double)
projectVertical (Hex i j) = (x * 3 / 2, - sqrt 3 * (x/2 - y))
  where
    x = fromIntegral i
    y = fromIntegral j


