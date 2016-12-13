
module Distance where

type DistanceFunction a  = [a] -> [a] -> a

euclidean a b = sqrt (sumOfSquare a b)
manhattan a b = sum $ zipWith (\x y -> abs(x - y)) a b

sumOfSquare a b = sum $ zipWith (\x y -> (x - y)^2) a b


