module Data.Graph.Grid.Vertex
(
    -- * Vertex
    Vertex(..),
    -- ** Enumerations
    allVertices,
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


-- | A vertex in an infinite regular planar graph. Every vertex in the graph
-- has the same number of neighbors. The set of vertices is recursively
-- enumerable.
--
class (Eq v, Monoid v) => Vertex v where

    -- | The number of neighbors of any vertex in the graph
    degree :: proxy v -> Int   

    -- | @distance a b@ is the positive integer distance between vertices @a@
    -- and @b@.
    --
    -- @
    -- distance a b  =  distance b a
    -- @
    --
    -- If there is no distance between two vertices, it is the same vertex.
    --
    -- @
    -- distance a b == 0  =  a == b
    -- @
    --
    distance  :: v -> v -> Integer

    -- | Enumerate all vertices at a given distance from the origin.
    --
    -- @
    -- v \`elem\` enumDistance d  =  distance mempty v == d
    -- @
    --
    -- @
    -- enumDistance 0  =  [mempty]
    -- @
    --
    enumDistance :: Integer -> [v]
    

-- | Enumerate all vertices in the graph, in ascending order of
-- distance from the origin. The resulting list is infinite.
allVertices :: (Vertex v) => [v]
allVertices = concatMap enumDistance [0..]


-- | Enumerate all vertices at a given distance or less from the origin.
--
-- @
-- enumRange i = takeWhile ((<= i) . distance) . allVertices
-- @
--
enumRange :: (Vertex v) => Integer -> [v]
enumRange = concatMap enumDistance . enumFromTo 0


-- | Determine if two vertices are adjacent.
--
-- @
-- adjacent a b  =  distance a b == 1
-- @
--
adjacent :: (Vertex v) => v -> v -> Bool
adjacent a b = distance a b == 1


-- | @neighbors v@ enumerates all vertices adjacent to @v@.
--
-- @
-- length (neighbors v)  =  degree v
-- @
--
-- @
-- a \`elem\` neighbors b  =  adjacent a b 
-- @
--
neighbors :: (Vertex v) => v -> [v]
neighbors v = map (v <>) (enumDistance 1)


-- TODO IMPROVE

-- | @closerTo d a b@ compares the distance of @a@ to @d@ to the distance
-- of @b@ to @d@.
closerTo :: (Vertex v) => v -> v -> v -> Ordering
closerTo to = comparing (distance to)


-- TODO IMPROVE

-- | @stepToward v s@ is the list of vertices adjacent to @s@ that are
-- closest to @d@. @stepToward a a == []@.
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

