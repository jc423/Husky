module KNN where

import Data.List as List
import Data.Ord (comparing)
import Distance

type Features = [Double]
data Classified a = Classified {features::Features,
                                label::a
                               }


--mostFrequent::Ord a => [(a,b)] -> b
--mostFrequent xs = snd $ head $ Map.toList $ List.foldl (\map (k, v) -> Map.insert k (currentValue k map) map) Map.empty xs
--  where currentValue k map = case Map.lookup k map of
--                               Nothing -> 0
--                               Just v -> v
  
knn:: DistanceFunction Features Features Double -> Int -> [Classified Double] -> Features -> Double
knn dist k train unknown = fst $ head $ take k $ tuple_qs $ List.map (\x -> (label x, dist (features x) unknown)) train

tuple_qs :: (Ord b) => [(a, b)] -> [(a, b)];
tuple_qs [] = [];
tuple_qs (x:xs) = [ lower | lower <- xs, (snd lower) <= (snd x) ] ++ [x] ++ [ higher | higher <- xs, (snd higher) >= (snd x) ];


unknownCar = [1979.0, 25.0, 1250.0]
cars = [Classified { features=[1982.0, 30.0, 1200.0], label=3000.0 },
        Classified { features=[1981.0, 20.0, 1300.0], label=2000.0 },
        Classified { features=[1983.0, 10.0, 1500.0], label=2000.0 },
        Classified { features=[1985.0, 5.0, 1100.0], label=1000.0 },
        Classified { features=[2011.0, 60.0, 120.0], label=10000.0 },
        Classified { features=[2010.0, 70.0, 130.0], label=10000.0 },
        Classified { features=[2015.0, 80.0, 140.0], label=10000.0 }];

unknownCarValue = knn euclidean 5 cars unknownCar
