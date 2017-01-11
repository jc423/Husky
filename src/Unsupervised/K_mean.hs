--input: number of training data, k centroid, k = 1 for now.
--require distance object
--calculate an array of mean, just do mean for now, an array of mean, min, max
--generate an random centroid of random value, initial zero for all value,
--take training data and find out which centroid is closest to it, 
--calculate distance, sort value, if head value = target, put into subset
--take subset of training data, calculate mean, regenerate centroid for next iteration
--main last centroid coordinate, calculate distance between new point and last point
--if distance delta is less than a threshold, return current point
--output centroid object
import Data.List;

getDist a b = sqrt $ sum [ (a !! idx - b !! idx)^2 | idx <- [0..(length a)-1]]

getDistList xs a = [getDist x a | x <- xs]

getNearSet ts cs idx = [ t | t <- ts, (getDist t c) == (minimum $ getDistList cs t)]

getNearSetC ts cs = [ getNearSet ts cs idx | idx <- [0..(length ts)-1] ]

getSumOfIdxElm xs idx len = sum [ x !! idx | x <- xs] / (fromIntegral len)
 
getMeanArr xs = [ getSumOfIdxElm xs idx rLen | idx <- [0..cLen-1]]
	where cLen = length $ xs !! 0
	where rLen = length xs

getNextCentroids ts cs = [ getMeanArr nearSet | nearSet <- getNearSetC ts cs ]


