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

mostCommon:: Ord a =>[(a, Double)] -> a
mostCommon xs = fst $ head $ head $ reverse $ sortBy (comparing length) $ groupBy ((==) `on` fst) $ sortBy (comparing fst) xs


knn:: Ord a => DistanceFunction Feature -> Int -> [Classified a] -> Features -> a
knn dist k train unknown = mostCommon $ take k $ Sort.tuple_qs $ List.map (\x -> (label x, dist (features x) unknown)) train


unknownCar1 = [2015.0, 75.0, 150.0]
unknownCar2 = [2015.0, 10.0, 1500.0]

carsPrice = [Classified { features=[1982.0, 30.0, 1200.0], label=3000.0 },
        Classified { features=[1981.0, 20.0, 1300.0], label=2000.0 },
        Classified { features=[1983.0, 10.0, 1500.0], label=2000.0 },
        Classified { features=[1985.0, 5.0, 1100.0], label=1000.0 },
        Classified { features=[2011.0, 60.0, 120.0], label=10000.0 },
        Classified { features=[2010.0, 70.0, 130.0], label=10000.0 },
        Classified { features=[2015.0, 80.0, 140.0], label=10000.0 }];

carsAge = [Classified { features=[1982.0, 30.0, 1200.0], label="ancient" },
        Classified { features=[1981.0, 20.0, 1300.0], label="old" },
        Classified { features=[1983.0, 10.0, 1500.0], label="old" },
        Classified { features=[1985.0, 5.0, 1100.0], label="old" },
        Classified { features=[2011.0, 60.0, 120.0], label="eh" },
        Classified { features=[2010.0, 70.0, 130.0], label="eh" },
        Classified { features=[2015.0, 80.0, 140.0], label="new" }];

-- examples

euclideanKNNPrice = knn euclidean 3 carsPrice
manhattanKNNPrice = knn manhattan 3 carsPrice
euclideanKNNAge = knn euclidean 3 carsAge
manhattanKNNAge = knn manhattan 3 carsAge

unknownCar1Price = euclideanKNNPrice unknownCar1
unknownCar1Age = euclideanKNNPrice unknownCar1
unknownCar2Price = euclideanKNNPrice unknownCar2
unknownCar2Age = euclideanKNNPrice unknownCar2

unknownCar1PriceM = manhattanKNNPrice unknownCar1
unknownCar1AgeM = manhattanKNNPrice unknownCar1
unknownCar2PriceM = manhattanKNNPrice unknownCar2
unknownCar2AgeM = manhattanKNNPrice unknownCar2
