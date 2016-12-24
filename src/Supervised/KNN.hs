module Supervised.KNN (
  Classified,
  Neighbor,
  weighted,
  mostCommon,
  knnClassification,
  knnRegression,
  euclideanKNNClassification,
  euclideanKNNRegression,
  cosineKNNClassification,
  cosineKNNRegression,
  manhattanKNNClassification,
  manhattanKNNRegression,
  KDTree,
  createKDTree,
  getNeighborhood,
  ) where

import Data.Function
import Data.List as List
import Data.Ord (comparing)
import HuskyML
import Distance
import Sort
-- | Represents an neighbor of unknown item                    
data Neighbor a = Neighbor {distance::Double,
                            labeled::a}

-- | Instance of Eq for Neighbor
instance Eq a => Eq (Neighbor a) where
  (==) (Neighbor d1 l1)  (Neighbor d2 l2) = l1 == l2

-- | Instance of Ord for Neighbor
instance Ord a => Ord (Neighbor a) where
  Neighbor d1 l1 `compare` Neighbor d2 l2 = d1 `compare` d2
  
-- | In determining the label assigns weight of each neighbor to 1/distance
weighted::Ord a =>
          [Neighbor a] -- ^ List of neighbors
        -> a -- ^ Return value
weighted xs = closestLabel $ map (aggregateDistanceForLabelGroup) $ groupByLabel xs
  where groupByLabel = group . sortBy (comparing labeled)
        aggregateDistanceForLabelGroup = \x -> Neighbor {labeled=labeled (head x), distance=(sum [1/(distance y + 0.000000000001)  | y <- x])} -- add to distance y to avoid divide by zero
        closestLabel = labeled . last . sortBy (comparing distance)

-- | Determines label by choosing most frequent label among neighbors
mostCommon:: Ord a => [Neighbor a] -- ^ List of neighbors
          -> a  -- ^ Return value
mostCommon xs = mostCommonLabel $ sortBy (comparing length) $ groupByLabel xs
  where groupByLabel = group . sortBy (comparing labeled)
        mostCommonLabel = labeled . head . last

-- | Nearest Neghbors
kNearestNeighbors :: (Ord a) => Int -- ^ Number of neighbors
                  -> DistanceFunction -- ^ Distance function
                 -> [Classified a] -- ^ List of training data
                 -> [Feature] -- ^ Unknown item
                 -> [Neighbor a] -- ^ returns nearest neighbors
kNearestNeighbors k distFunc training unknown = takeKNearest $ List.map (\x -> Neighbor{labeled=(label x),distance=(distFunc (features x) unknown)}) training                
  where takeKNearest = take k . sort
  
-- | KNN Classification implementation that offers flexibility in distance calculation, number of neighbors and weight function
knnClassification:: (Ord a) => DistanceFunction -- ^ Distance function
   -> Int -- ^ Number of neighbors to use
   -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
   -> [Classified a] -- ^ List of training data
   -> [Feature] -- ^ Unknown item
   -> a -- ^ Label for unknown item
knnClassification dist k weightFn train unknown = weightFn $ kNearestNeighbors k dist train unknown

-- | KNN Regression implementation that offers flexibility in distance calculation, number of neighbors and weight function
knnRegression:: (Ord a, Fractional a) => DistanceFunction -- ^ Distance function
   -> Int -- ^ Number of neighbors to use
   -> [Classified a] -- ^ List of training data
   -> [Feature] -- ^ Unknown item
   -> a -- ^ Label for unknown item
knnRegression dist k train unknown = avgOfNeighbors $ kNearestNeighbors k dist train unknown
  where avgOfNeighbors = (/ (fromIntegral k)) . sum . map (\x -> labeled x)


-- | KNN using cosine distance
cosineKNNClassification::(Ord a) => Int -- ^ Number of neighbors
         -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
         -> [Classified a] -- ^ List of training data
         -> [Feature] -- ^ Unknown item
         -> a -- ^ Label
cosineKNNClassification = knnClassification cosineDistance

-- | KNN regression using cosine distance
cosineKNNRegression::(Ord a, Fractional a) => Int -- ^ Number of neighbors
         -> [Classified a] -- ^ List of training data
         -> [Feature] -- ^ Unknown item
         -> a -- ^ Label
cosineKNNRegression = knnRegression cosineDistance


-- | KNN using euclidean distance
euclideanKNNClassification::(Ord a) => Int -- ^ Number of neighbors
         -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
         -> [Classified a] -- ^ List of training data
         -> [Feature] -- ^ Unknown item
         -> a -- ^ Label
euclideanKNNClassification = knnClassification euclidean

-- | KNN regression using euclidean distance
euclideanKNNRegression::(Ord a, Fractional a) => Int -- ^ Number of neighbors
         -> [Classified a] -- ^ List of training data
         -> [Feature] -- ^ Unknown item
         -> a -- ^ Label
euclideanKNNRegression = knnRegression euclidean

-- | KNN using manhattan distance
manhattanKNNClassification::(Ord a) => Int -- ^ Number of neighbors
         -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
         -> [Classified a] -- ^ List of training data
         -> [Feature] -- ^ Unknown item
         -> a -- ^ Label
manhattanKNNClassification = knnClassification manhattan

-- | KNN regression using manahattan distance
manhattanKNNRegression::(Ord a, Fractional a) => Int -- ^ Number of neighbors
         -> [Classified a] -- ^ List of training data
         -> [Feature] -- ^ Unknown item
         -> a -- ^ Label
manhattanKNNRegression = knnRegression manhattan

-- KD Tree optimization

data KDTree a = Node {left::KDTree a, right::KDTree a, this::Classified a}  | Leaf [Classified a] deriving (Show)

-- | KD tree implementation.  Attribute chosen is increased by one for each level.
createKDTree :: (Ord a) => [Classified a] -- ^ List of training data
             -> Int -- ^ index of intial attribute to split on
             -> Int -- ^ number of level in tree
             -> KDTree a -- ^ returns KDTree
createKDTree [] _ _ = Leaf []
createKDTree train _ 0 = Leaf train
createKDTree train fIndex levels
  | fIndex + 1 > (length $ features (train !! 0)) = Leaf []
  | otherwise =  Node {left=leftTree, right=rightTree, this=current}
  where sortedTraining = sortTraining train fIndex
        midpoint = (length sortedTraining) `div` 2
        leftTree = createKDTree (take midpoint sortedTraining) (fIndex + 1) (levels - 1)
        rightTree = createKDTree (drop (midpoint + 1) sortedTraining) (fIndex + 1) (levels - 1)
        current = sortedTraining !! midpoint

-- | For an unknown point gets neighborhood to check against.  This decreases complexity because not all training data checked against.
getNeighborhood :: KDTree a -- ^ KDTree
                -> [Feature] -- ^ Unknown item
                -> Int -- ^ Index of initial attribute to check
                -> [Classified a] -- ^ Neighborhood of training data
getNeighborhood kdTree unknown fIndex = case kdTree of
  Node lft rgt this -> case (unknown !! fIndex) < ((!! fIndex) $ features this) of
                          True -> getNeighborhood lft unknown (fIndex + 1)
                          False -> getNeighborhood rgt unknown (fIndex + 1)
  Leaf neighborhood -> neighborhood

sortTraining :: [Classified a] -> Int -> [Classified a]
sortTraining train fIndex = sortBy (comparing ((!! fIndex) . features)) train
