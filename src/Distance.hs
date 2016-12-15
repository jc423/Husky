module Distance where

type DistanceFunction a  = [a] -> [a] -> a

minkowski :: (Ord a, Floating a) => Int -> DistanceFunction a
minkowski p xs ys = (**) (sum $ zipWith (\x y -> abs(x - y) ^ p) xs ys) (1.0 / fromIntegral p)

euclidean :: (Ord a,Floating a) => DistanceFunction a
euclidean xs ys = minkowski 2 xs ys

manhattan :: (Ord a, Floating a) => DistanceFunction a
manhattan xs ys = minkowski 1 xs ys

-- adding  to -1 is to account for cosine similarity of 1 meaning same orientation, -1 opposite and 0 orthogonal
cosineDistance ::(Floating a) => DistanceFunction a
cosineDistance xs ys = abs $ (+) (-1) $ (/) (sum $ zipWith (\x y -> x * y) xs ys) ((magnitude xs) * (magnitude ys))


sumOfSquare xs ys = sum $ zipWith (\x y -> (x - y)^2) xs ys
magnitude xs = sqrt $ sum $ zipWith (\x y -> x * y) xs xs


