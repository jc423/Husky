module Supervised.KNN (Classified, Neighbor, weighted, mostCommon, knn, euclideanKNN, cosineKNN, manhattanKNN) where

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
weighted xs = labeled $ last $ sortBy (comparing distance)$ map (\x -> Neighbor {labeled=labeled (head x), distance=(sum [1/distance y | y <- x])}) $ group $ sortBy (comparing labeled) xs

-- | Determines label by choosing most frequent label among neighbors
mostCommon:: Ord a => [Neighbor a] -- ^ List of neighbors
          -> a  -- ^ Return value
mostCommon xs = labeled $ head $ last $ sortBy (comparing length) $ group $ sortBy (comparing labeled) xs

-- | KNN implementation that offers flexibility in distance calculation, number of neighbors and weight function
knn:: (Ord a, Ord b, Attribute b) => DistanceFunction b -- ^ Distance function
   -> Int -- ^ Number of neighbors to use
   -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
   -> [Classified a b] -- ^ List of training data
   -> [b] -- ^ Unknown item
   -> a -- ^ Label for unknown item
knn dist k weightFn train unknown = weightFn $ take k $ sort $ List.map (\x -> Neighbor{labeled=(label x),distance=(dist (features x) unknown)}) train

-- | KNN using cosine distance
cosineKNN::(Ord a, Ord b, Attribute b) => Int -- ^ Number of neighbors
         -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
         -> [Classified a b] -- ^ List of training data
         -> [b] -- ^ Unknown item
         -> a -- ^ Label
cosineKNN = knn cosineDistance

-- | KNN using euclidean distance
euclideanKNN::(Ord a, Ord b, Attribute b) => Int -- ^ Number of neighbors
         -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
         -> [Classified a b] -- ^ List of training data
         -> [b] -- ^ Unknown item
         -> a -- ^ Label
euclideanKNN = knn euclidean

-- | KNN using manhattan distance
manhattanKNN::(Ord a, Ord b, Attribute b) => Int -- ^ Number of neighbors
         -> ([Neighbor a] -> a) -- ^ Function for assigning weights to neighbors
         -> [Classified a b] -- ^ List of training data
         -> [b] -- ^ Unknown item
         -> a -- ^ Label
manhattanKNN = knn manhattan

-- examples
  
unknownCar1 = [6030.0, 160.0, 280.0]
unknownCar2 = [2015.0, 10.0, 1500.0]

carsPrice = [Classified { features=[1982.0, 30.0, 1200.0], label=3000.0 },
        Classified { features=[1981.0, 20.0, 1300.0], label=2000.0 },
        Classified { features=[1983.0, 10.0, 1500.0], label=2000.0 },
        Classified { features=[1985.0, 5.0, 1100.0], label=1000.0 },
        Classified { features=[2011.0, 60.0, 120.0], label=10000.0 },
        Classified { features=[2010.0, 70.0, 130.0], label=10000.0 },
        Classified { features=[3015.0, 80.0, 140.0], label=30000.0 }];

carsAge = [Classified { features=[1982.0, 30.0, 1200.0], label="ancient" },
        Classified { features=[1981.0, 20.0, 1300.0], label="old" },
        Classified { features=[1983.0, 10.0, 1500.0], label="old" },
        Classified { features=[1985.0, 5.0, 1000.0], label="old" },
        Classified { features=[2011.0, 60.0, 120.0], label="eh" },
        Classified { features=[2010.0, 70.0, 130.0], label="eh" },
        Classified { features=[2010.0, 50.0, 140.0], label="new" }];



