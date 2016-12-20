module Distance (DistanceFunction, minkowski, euclidean, manhattan, cosineDistance) where

import HuskyML

-- | Given two vectors returns a distance
type DistanceFunction a = [a] -> [a] -> Double

-- | Given two vectors and a degree returns the minkowski distance
minkowski :: (Ord a, Attribute a) => Int -> DistanceFunction a
minkowski p xs ys = (**) (sum $ zipWith (\x y -> abs(x `diff` y) ^ p) xs ys) (1.0 / fromIntegral p)

-- | Given two vectors returns the euclidean distance
euclidean :: (Ord a, Attribute a) => DistanceFunction a
euclidean xs ys = minkowski 2 xs ys

-- | Given two vectors returns the manhattan distance
manhattan :: (Ord a, Attribute a) => DistanceFunction a
manhattan xs ys = minkowski 1 xs ys

-- | Given two vectors returns the absolute value of the cosine similarity minus one to account for 1 meaning same orientation, -1 opposite and 0 meaning orthogonal
cosineDistance ::(Ord a, Attribute a) => DistanceFunction a
cosineDistance xs ys = abs $ (+) (-1) $ (/) (sum $ zipWith (\x y -> x `times` y) xs ys) ((magnitude xs) `times` (magnitude ys))

magnitude xs = sqrt $ sum $ zipWith (\x y -> x `times` y) xs xs


