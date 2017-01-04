module TaskB where

zipN :: ([a] -> b) -> [[a]] -> [b]
zipN f list = map f $ transpose list

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose l = (map head l) : transpose (map tail l)