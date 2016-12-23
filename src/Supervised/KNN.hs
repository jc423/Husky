module Supervised.KNN (Classified, Neighbor, weighted, mostCommon, knnClassification, euclideanKNN, cosineKNN, manhattanKNN) where

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
        aggregateDistanceForLabelGroup = \x -> Neighbor {labeled=labeled (head x), distance=(sum [1/distance y + 1 | y <- x])}
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
  
-- | KNN implementation that offers flexibility in distance calculation, number of neighbors and weight function
knnClassification:: (Ord a) => DistanceFunction -- ^ Distance function
   -> Int -- ^ Number of neighbors to use
   -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
   -> [Classified a] -- ^ List of training data
   -> [Feature] -- ^ Unknown item
   -> a -- ^ Label for unknown item
knnClassification dist k weightFn train unknown = weightFn $ kNearestNeighbors k dist train unknown


-- | KNN using cosine distance
cosineKNN::(Ord a) => Int -- ^ Number of neighbors
         -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
         -> [Classified a] -- ^ List of training data
         -> [Feature] -- ^ Unknown item
         -> a -- ^ Label
cosineKNN = knnClassification cosineDistance

-- | KNN using euclidean distance
euclideanKNN::(Ord a) => Int -- ^ Number of neighbors
         -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
         -> [Classified a] -- ^ List of training data
         -> [Feature] -- ^ Unknown item
         -> a -- ^ Label
euclideanKNN = knnClassification euclidean

-- | KNN using manhattan distance
manhattanKNN::(Ord a) => Int -- ^ Number of neighbors
         -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
         -> [Classified a] -- ^ List of training data
         -> [Feature] -- ^ Unknown item
         -> a -- ^ Label
manhattanKNN = knnClassification manhattan

-- examples
  
unknownCar1 = [FDouble 6030.0, FString "Toyota", FInt 280]
unknownCar2 = [FDouble 2015.0, FString "Ford", FInt 1500]

carsPrice = [Classified { features=[FDouble 1982.0, FString "Toyota", FInt 1200], label=3000.0 },
        Classified { features=[FDouble 1981.0, FString "Toyota", FInt 1300], label=2000.0 },
        Classified { features=[FDouble 1983.0, FString "Nissan", FInt 1500], label=2000.0 },
        Classified { features=[FDouble 1985.0, FString "Toyota", FInt 1100], label=1000.0 },
        Classified { features=[FDouble 2011.0, FString "Jaguar", FInt 120], label=10000.0 },
        Classified { features=[FDouble 2010.0, FString "Honda", FInt 130], label=10000.0 },
        Classified { features=[FDouble 3015.0, FString "Subaru", FInt 140], label=30000.0 }];

carsAge = [Classified { features=[FDouble 1982.0, FString "Toyota", FInt 1200], label="ancient" },
        Classified { features=[FDouble 1981.0, FString "Toyota", FInt 1300], label="ancient" },
        Classified { features=[FDouble 1983.0, FString "Nissan", FInt 1500], label="ancient" },
        Classified { features=[FDouble 1985.0, FString "Toyota", FInt 1100], label="old" },
        Classified { features=[FDouble 2011.0, FString "Jaguar", FInt 120], label="eh" },
        Classified { features=[FDouble 2010.0, FString "Honda", FInt 130], label="eh" },
        Classified { features=[FDouble 3015.0, FString "Subaru", FInt 140], label="new" }];


