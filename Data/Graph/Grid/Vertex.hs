module Data.Graph.Grid.Vertex
(
    -- * Vertex
    Vertex,
    distance,
    enumDistance,
    -- ** Enumerations
    allIndices,
    enumRange,
    -- ** Neighboring tiles
    neighbors,
    adjacent,
    -- ** Comparison
    closerTo,
    -- ** Paths
    stepToward,
    pathsToward,
)
where

import Data.Monoid
import Data.Ord ( comparing )


class (Eq v, Monoid v) => Vertex v where

    -- | @distance a b == 0@ iff @a == b@
    distance  :: v -> v -> Integer

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
allIndices :: (Vertex v) => [v]
allIndices = concatMap enumDistance [0..]

-- | Return a finite list of all indices at a given distance or less from
-- the origin.
enumRange :: (Vertex v) => Integer -> [v]
enumRange = concatMap enumDistance . enumFromTo 0

-- | Determine if two tiles are adjacent.
adjacent :: (Vertex v) => v -> v -> Bool
adjacent a b = distance a b == 1

-- | @neighbors v@ enumerates every value @n@ for which @adjacent v n@.
neighbors :: (Vertex v) => v -> [v]
neighbors v = map (v <>) (enumDistance 1)


-- TODO IMPROVE

-- | @closerTo d a b@ compares the distance of @a@ to @d@ to the distance
-- of @b@ to @d@.
closerTo :: (Vertex v) => v -> v -> v -> Ordering
closerTo to = comparing (distance to)


-- TODO IMPROVE

-- | @stepToward d s@ is the list of tiles adjacent to @s@ that are of
-- minimal distance to @d@. @stepToward a a == []@.
stepToward :: (Vertex v) => v -> v -> [v]
stepToward to from
  | to == from = []
  | otherwise = closest
  where
    closest = minimumsWith (distance to) $ from : neighbors from

-- | @pathsToward d s@ is the list of all minimal paths from @s@ to @d@.
pathsToward :: (Vertex v) => v -> v -> [[v]]
pathsToward to = map stepPath . stepToward to
  where
    stepPath s = concatMap (s:) $ pathsToward to s



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

