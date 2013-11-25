module Data.Tile
(
-- * Tile
  Tile
, distance
, enumDistance
-- ** Enumerations
, allIndices
, enumRange
-- ** Neighboring tiles
, neighbors
, adjacent
-- ** Comparison
, closerTo
-- ** Paths
, stepToward
, pathsToward
-- * Tiling types
-- ** Quad
, Quad(..)
-- ** Oct
, Oct(..)
-- ** Hex
, Hex(..)
-- * Projections
, Project(..)
, projectHorizontal
, projectVertical
)
where

import Data.Monoid
import Data.Ord ( comparing )
import Data.Ix
import Data.Hashable


class (Eq v, Monoid v) => Tile v where

    -- | @distance a b == 0@ iff @a == b@
    distance :: v -> v -> Integer

    -- | Return a finite list of all indices at a given distance from the
    -- origin. @t `elem` enumDistance i@ iff @distance mempty t == i@.
    --
    -- @
    -- enumDistance 0 = [mempty]
    -- @
    --
    enumDistance :: Integer -> [v]


-- | Enumerate all indices in the coordinate system, in ascending order of
-- distance from the origin. The resulting list is infinite.
allIndices :: (Tile v) => [v]
allIndices = concatMap enumDistance [0..]

-- | Return a finite list of all indices at a given distance or less from
-- the origin.
enumRange :: (Tile v) => Integer -> [v]
enumRange = concatMap enumDistance . enumFromTo 0

-- | Determine if two tiles are adjacent.
adjacent :: (Tile v) => v -> v -> Bool
adjacent a b = distance a b == 1

-- | @neighbors v@ enumerates every value @n@ for which @adjacent v n@.
neighbors :: (Tile v) => v -> [v]
neighbors v = map (v <>) (enumDistance 1)


-- TODO IMPROVE

-- | @closerTo d a b@ compares the distance of @a@ to @d@ to the distance
-- of @b@ to @d@.
closerTo :: (Tile v) => v -> v -> v -> Ordering
closerTo to = comparing (distance to)


-- TODO IMPROVE

-- | @stepToward d s@ is the list of tiles adjacent to @s@ that are of
-- minimal distance to @d@. @stepToward a a == []@.
stepToward :: (Tile v) => v -> v -> [v]
stepToward to from
  | to == from = []
  | otherwise = closest
  where
    closest = minimumsWith (distance to) $ from : neighbors from

-- | @pathsToward d s@ is the list of all minimal paths from @s@ to @d@.
pathsToward :: (Tile v) => v -> v -> [[v]]
pathsToward to = map stepPath . stepToward to
  where
    stepPath s = concatMap (s:) $ pathsToward to s


-------------------------------------------------------------------------------
-- Quad tilings
-------------------------------------------------------------------------------

-- | A tile with four neighbors; rectilinear distance.
data Quad = Quad !Integer !Integer
    deriving (Show, Eq, Ord, Ix)

instance Monoid Quad where
    mempty = Quad 0 0
    Quad x y `mappend` Quad x' y' = Quad (x+x') (y+y')

instance Hashable Quad where
    hashWithSalt s (Quad i j) = hashWithSalt s (i, j)

instance Tile Quad where
    distance (Quad x y) (Quad x' y') = abs (x - x') + abs (y - y')

    enumDistance 0 = [Quad 0 0]
    enumDistance n =
        [Quad i (n - abs i) | i <- reverse range]
        ++ init (drop 1 [Quad i (-n + abs i) | i <- range])
      where
        range = [-n..n]


-------------------------------------------------------------------------------
-- Oct tilings
-------------------------------------------------------------------------------

-- | A tile with eight neighbors; chebyshev distance.
data Oct = Oct !Integer !Integer
    deriving (Show, Eq, Ord, Ix)

instance Monoid Oct where
    mempty = Oct 0 0
    Oct x y `mappend` Oct x' y' = Oct (x+x') (y+y')

instance Hashable Oct where
    hashWithSalt s (Oct i j) = hashWithSalt s (i, j)

instance Tile Oct where
    distance (Oct x y) (Oct x' y') = max (abs $ x - x') (abs $ y - y')

    -- TODO: implement enumDistance

-------------------------------------------------------------------------------
-- Hexagonal tilings
-------------------------------------------------------------------------------

data Hex = Hex !Integer !Integer
    deriving (Show, Eq, Ord, Ix)

instance Monoid Hex where
    mempty = Hex 0 0
    Hex x y `mappend` Hex x' y' = Hex (x+x') (y+y')

instance Hashable Hex where
    hashWithSalt s (Hex i j) = hashWithSalt s (i, j)
    
instance Tile Hex where
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


-------------------------------------------------------------------------------
-- Euclidian plane projections
-------------------------------------------------------------------------------

-- | Class for types that have a canonical position on the Euclidian plane
class Project a where
    project :: a -> (Double, Double)
    queryPoint :: (Double, Double) -> a
    queryRect :: (Double, Double) -> (Double, Double) -> [a]

instance Project Quad where
    project (Quad x y) = (fromIntegral x, fromIntegral y)
    queryPoint (x, y) = Quad (round x) (round y)
    queryRect a b = range (queryPoint a, queryPoint b)

instance Project Oct where
    project (Oct x y) = (fromIntegral x, fromIntegral y)
    queryPoint (x, y) = Oct (round x) (round y)
    queryRect a b = range (queryPoint a, queryPoint b)


newtype HexH = HexH { unHexH :: Hex }
    deriving (Show, Eq, Ord)

instance Project HexH where
    project = projectHorizontal . unHexH


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


-------------------------------------------------------------------------------
-- internal utilities
-------------------------------------------------------------------------------

-- | @minimumsWith f xs@ is the list of minimum values @f x@ for every @x@ in
-- @xs@.
--
-- example:
--
-- @
-- minimumsWith length ["three", "four", "five"]  =  ["four", "five"]
-- @
minimumsWith :: (Ord b) => (a -> b) -> [a] -> [a]
minimumsWith f = minimumsBy $ comparing f

minimumsBy :: (a -> a -> Ordering) -> [a] -> [a]
minimumsBy _ [] = []
minimumsBy f (x:xs) = reverse $ go x [] xs
  where
    go a ms [] = a:ms
    go a ms (x:xs) =
        case f a x of
            EQ -> go x (a:ms) xs
            LT -> go a ms xs
            GT -> go x [] xs

