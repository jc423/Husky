
module Distance where

type DistanceFunction a  = [a] -> [a] -> a

euclidean :: Floating a => DistanceFunction a
euclidean a b = sqrt (sumOfSquare a b)

manhattan :: Floating a => DistanceFunction a
manhattan a b = sum $ zipWith (\x y -> abs(x - y)) a b

-- adding  to -1 is to account for cosine similarity of 1 meaning same orientation, -1 opposite and 0 orthogonal
cosineDistance ::(Floating a) => DistanceFunction a
cosineDistance a b = (+) (-1) $ (/) (sum $ zipWith (\x y -> x * y) a b) ((magnitude a) * (magnitude b))

sumOfSquare a b = sum $ zipWith (\x y -> (x - y)^2) a b
magnitude a = sqrt $ sum $ zipWith (\x y -> x * y) a a


