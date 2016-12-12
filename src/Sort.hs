module Sort where

tuple_qs :: (Ord b) => [(a, b)] -> [(a, b)];
tuple_qs [] = [];
tuple_qs (x:xs) = [ lower | lower <- xs, (snd lower) <= (snd x) ] ++ [x] ++ [ higher | higher <- xs, (snd higher) >= (snd x) ];
