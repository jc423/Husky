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

--TDL: 
--1) write test, provide seed data
--2) change function to accept distance function
--3) abstract iterative array like operation to accept a collback instead
--4) change function to use actual data structue instead of just list of list
--5) change getMean function to a "getAggerate" function that can do both mean and medoum
--6) add logging

getDist :: (Double a) => [a] -> [a] -> a
getDist a b = sqrt $ sum [ (a !! idx - b !! idx)^2 | idx <- [0..(length a)-1]]

getElementDist :: (Double b) => [a] -> [a] -> [b]
getElementDist xs ys = [getDist (xs !! idx) (ys !! idx) | idx <- [0..(length xs)-1]]

-- get a list of distance for between a point and a list of points
getDistList :: [a] -> a -> [a]
getDistList xs a = [getDist x a | x <- xs]

getNearSet :: (Int b) => [a] -> [a] -> b -> [a]
getNearSet ts cs idx = [ t | t <- ts, (getDist t c) == miniDistance]
	where miniDistance = minimum $ getDistList cs t

getNearSetsForCentroid ::[a] -> [a] -> [a]
getNearSetsForCentroid ts cs = [ getNearSet ts cs idx | idx <- [0..(length ts)-1] ]

getMeanOfIdxElm :: (Double b, Int i) => [a] -> i -> i -> b
getMeanOfIdxElm xs idx len = sum [ x !! idx | x <- xs] / (fromIntegral len)
 
getMeanArr :: (Double b) => [a] -> [b]
getMeanArr xs = [ getMeanOfIdxElm xs idx rLen | idx <- [0..cLen-1]]
	where cLen = length $ xs !! 0
	where rLen = length xs

getNextCentroids :: [a] -> [a] -> [a]
getNextCentroids ts cs = [ getMeanArr nearSet | nearSet <- getNearSetsForCentroid ts cs ]

-- input threshold level, during each iteration of the centroid generation
-- it will check to see if threshold for all centroids are met
-- conitune until threshold are met

findKmeanCluster :: (Double b) => [a] -> [a] -> b -> [a]
findKmeanCluster ts cs threshold =
	| (length $ [1 | delta <- deltas, delta <= threshold]) == centroidsLength = nextCentroids
	| otherwise = findKmeanCluster ts (nextCentroids) threshold
	let nextCentroids = getNextCentroids ts cs    
	in deltas = getElementDist nextCentroids cs
	where centroidsLength = length cs