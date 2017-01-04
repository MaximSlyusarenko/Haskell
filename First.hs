module FirstHW where

getWordValue :: String -> Int
getWordValue ('+':xs) = 
    if tmp < 0
    then error "Incorrect number"
    else tmp
    where
    tmp = read xs
getWordValue s = read s

stringSum :: String -> Int
stringSum s = sum $ map getWordValue $ words s

---------------------------------------------------------------------------------------

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose l = (map head l) : transpose (map tail l)

zipN :: ([a] -> b) -> [[a]] -> [b]
zipN f list = map f $ transpose list

---------------------------------------------------------------------------------------

mergeSort :: [Int] -> [Int]
mergeSort (x:[]) = (x:[])
mergeSort [] = []
mergeSort l = merge (mergeSort $ fst a) (mergeSort $ snd a) where a = splitAt (div (length l) 2) l

merge :: [Int] -> [Int] -> [Int]
merge a@(x:xs) b@(y:ys) = 
    if (x <= y)
    then x:(merge xs b)
    else y:(merge a ys)
merge l [] = l
merge [] l = l