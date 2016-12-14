module Supervised.KNN where

import Data.Function
import Data.List as List
import Data.Ord (comparing)
import Distance
import Sort

type Feature = Double
type Features = [Feature]

data Classified a = Classified {features::Features,
                                label::a
                               }
data Neighbor a = Neighbor {distance::Double,
                            labeled::a}

instance Eq a => Eq (Neighbor a) where
  (==) (Neighbor d1 l1)  (Neighbor d2 l2) = l1 == l2

-- why do we need Ord a when compare is on distance
instance Ord a => Ord (Neighbor a) where
  Neighbor d1 l1 `compare` Neighbor d2 l2 = d1 `compare` d2
  

weighted::Ord a => [Neighbor a] -> a
weighted xs = labeled $ last $ sortBy (comparing distance)$ map (\x -> Neighbor {labeled=labeled (head x), distance=(sum [1/distance y | y <- x])}) $ group $ sortBy (comparing labeled) xs

mostCommon:: Ord a => [Neighbor a] -> a
mostCommon xs = labeled $ head $ last $ sortBy (comparing length) $ group $ sortBy (comparing labeled) xs


knn:: Ord a => DistanceFunction Feature -> Int -> ([Neighbor a] -> a) -> [Classified a] -> Features -> a
knn dist k weightFn train unknown = weightFn $ take k $ sort $ List.map (\x -> Neighbor{labeled=(label x),distance=(dist (features x) unknown)}) train


unknownCar1 = [2015.0, 75.0, 150.0]
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
        Classified { features=[1985.0, 5.0, 1100.0], label="old" },
        Classified { features=[2011.0, 60.0, 120.0], label="eh" },
        Classified { features=[2010.0, 70.0, 130.0], label="eh" },
        Classified { features=[2015.0, 80.0, 140.0], label="new" }];

-- examples

