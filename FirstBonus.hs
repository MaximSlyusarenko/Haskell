module FirstBonus where

import TreePrinters
       ( Tree
       , Tree(Node)
       , Tree(Leaf)
       , verticalPrint)

split :: Ord a => Tree (Int, a) -> Int -> (Tree (Int, a), Tree (Int, a))
split Leaf _ = (Leaf, Leaf)
split (Node (size, x) left right) k = 
    if getSize left >= k
    then (fst spl, Node (getSizeForParent (snd spl) right, x) (snd spl) right)
    else (Node (getSizeForParent left (fst splitted), x) left (fst splitted), snd splitted)
  where
    splitted = split right (k - (getSize left) - 1)
    spl = split left k


getSize :: Ord a => Tree (Int, a) -> Int
getSize Leaf = 0
getSize (Node (size, _) _ _) = size

getSizeForParent :: Ord a => Tree (Int, a) -> Tree (Int, a) -> Int
getSizeForParent left right = getSize left + getSize right + 1

merge :: Ord a => Tree (Int, a) -> Tree (Int, a) -> Tree (Int, a)
merge tree Leaf = tree
merge Leaf tree = tree
merge t1@(Node (x1, y1) left1 right1) t2@(Node (x2, y2) left2 right2) =
    if y1 > y2
    then Node (getSizeForParent left1 merge1, y1) left1 (merge right1 t2)
    else Node (getSizeForParent merge2 right2, y2) (merge t1 left2) right2
  where
    merge1 = merge right1 t2
    merge2 = merge t1 left2 

insertDecart :: Ord a => Tree (Int, a) -> a -> Int -> Tree (Int, a)
insertDecart tree element position = merge (merge t1 (Node (1, element) Leaf Leaf)) t2
  where
    spl = split tree position
    t1 = fst spl
    t2 = snd spl

removeDecart :: Ord a => Tree (Int, a) -> Int -> Tree (Int, a)
removeDecart Leaf _ = Leaf
removeDecart (Node (x1, y) left right) x = 
    if x > x1
    then Node (x1, y) left (removeDecart right x)
    else 
        if x < x1
        then Node (x1, y) (removeDecart left x) right
        else merge left right

fromListDecart :: Ord a => [a] -> Tree (Int, a)
fromListDecart list = fromListDecartWithCurTree list Leaf 1

fromListDecartWithCurTree :: Ord a => [a] -> Tree (Int, a) -> Int -> Tree (Int, a)
fromListDecartWithCurTree [] curTree _ = curTree
fromListDecartWithCurTree (x:xs) curTree curPos = 
    fromListDecartWithCurTree xs (merge curTree (Node (1, x) Leaf Leaf)) (curPos + 1)

rmqDecart :: Ord a => Tree (Int, a) -> Int -> Int -> Maybe a
rmqDecart tree beginIndex endIndex = 
    case secondSplit of
        Leaf            -> Nothing
        Node (_, x) _ _ -> Just x
  where
    firstSplit = snd (split tree (beginIndex - 1)) -- TODO: check
    secondSplit = fst (split firstSplit (endIndex - beginIndex + 1))