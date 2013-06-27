module Data.Tile where

import Data.Monoid
import Data.Ord ( comparing )




class (Eq v, Monoid v) => Tile v where
    -- | @distance a b == 0@ iff @a == b@
    distance :: v -> v -> Int
    -- | Return a finite list of all indices at a given distance from the
    -- origin.
    enumDistance :: Int -> [v]


-- | List all indices in the coordinate system.
allIndices :: (Tile v) => [v]
allIndices = concatMap enumDistance [0..]

-- | Return a finite list of all indices at a given distance or less from
-- the origin.
enumRange :: (Tile v) => Int -> [v]
enumRange = concatMap enumDistance . enumFromTo 0

-- | Determine if two indices are adjacent.
adjacent :: (Tile v) => v -> v -> Bool
adjacent a b = distance a b == 1

-- | @neighbors v@ enumerates every value @n@ for which @adjacent v n@.
neighbors :: (Tile v) => v -> [v]
neighbors v = map (v <>) (enumDistance 1)

-- TODO IMPROVE
closerTo :: (Tile v) => v -> v -> v -> Ordering
closerTo to = comparing (distance to)

-- TODO IMPROVE
stepToward :: (Tile v) => v -> v -> [v]
stepToward to from
  | to == from = []
  | otherwise = closest
  where
    closest = minimumsWith (distance to) $ from : neighbors from

pathsToward :: (Tile v) => v -> v -> [[v]]
pathsToward to = map stepPath . stepToward to
  where
    stepPath s = concatMap (s:) $ pathsToward to s



minimums :: (Ord a) => [a] -> [a]
minimums = minimumsBy compare

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




data Hex = Hex !Int !Int
    deriving (Show, Eq, Ord)

unHex :: Hex -> (Int, Int)
unHex (Hex i j) = (i, j)


instance Monoid Hex where
    mempty = Hex 0 0
    Hex x y `mappend` Hex x' y' = Hex (x+x') (y+y')
    
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
        relativePath = init $ concatMap (replicate n)
            [ Hex (-1) (-1)
            , Hex (-1)   0
            , Hex   0    1
            , Hex   1    1
            , Hex   1    0
            , Hex   0  (-1)
            ]

-- | Determine the cartesian position of a Hex in a horizontal grid
projectHorizontal :: Hex -> (Double, Double)
projectHorizontal (Hex i j) = (- sqrt 3 * (y/2 - x), y * 3 / 2)
  where
    x = fromIntegral i
    y = fromIntegral j
    v = 1.366025  -- distance between two tiles along skewed dimension

projectVertical :: Hex -> (Double, Double)
projectVertical (Hex i j) = (x * 3 / 2, - sqrt 3 * (x/2 - y))
  where
    x = fromIntegral i
    y = fromIntegral j


{-
data Grid v = Grid
    { contains  :: v -> Bool
    , generator :: Maybe (Generate v)
    } 

data Generate v = G
    { eSize :: Int
    , eList :: [v]
    }

total :: (Tile v) => Grid v
total = Grid (const True) Nothing

filterGrid :: (v -> Bool) -> Grid v -> Grid v
filterGrid p (Grid c g) = Grid (\v -> p v && c v) g


bounded :: Grid v -> Bool
bounded = isJust . generator

-- | The size of the Grid.
--
-- @size g = undefined@ iff @bounded g = False@
size :: Grid v -> Int
size g = case generator g of
             Just gen -> eSize gen
             Nothing  -> undefined
-}
