module FourthHW where

data InNode a = Node 
    { label :: a
    , parent :: Maybe (InNode a) 
    } deriving Show

leastCommonAncestor :: Eq a => InNode a -> InNode a -> Maybe (InNode a)
leastCommonAncestor node1 node2 = 
    leastCommonAncestorWithLists node1 node2 [node1] [node2]

leastCommonAncestorWithLists :: Eq a => InNode a -> InNode a -> [InNode a] -> [InNode a] -> Maybe (InNode a)
leastCommonAncestorWithLists node1 node2 list1 list2
    | containsLabel list2 $ label node1 = Just node1
    | containsLabel list1 $ label node2 = Just node2
    | otherwise                         = 
        shouldContinue parent1 parent2 >>=
            \_ -> leastCommonAncestorWithLists (getNewNode node1 parent1) (getNewNode node2 parent2)
            (getNewList list1 (parent1 >>= (\x -> return $ x:list1))) 
            (getNewList list2 (parent2 >>= (\x -> return $ x:list2)))
  where
    parent1 = parent node1
    parent2 = parent node2

shouldContinue :: Eq a => Maybe (InNode a) -> Maybe (InNode a) -> Maybe Bool
shouldContinue Nothing Nothing = Nothing
shouldContinue _ _ = Just True

getNewNode :: Eq a => InNode a -> Maybe (InNode a) -> InNode a
getNewNode oldNode Nothing = oldNode
getNewNode _ (Just newNode) = newNode

getNewList :: Eq a => [InNode a] -> Maybe [InNode a] -> [InNode a]
getNewList oldList Nothing = oldList
getNewList _ (Just newList) = newList

containsLabel :: Eq a => [InNode a] -> a -> Bool
containsLabel list elemt = elemt `elem` map label list

testNode :: InNode Int
testNode = Node 10 $ Just (Node 5 Nothing)

testNode1 :: InNode Int
testNode1 = Node 15 $ Just testNode

testNode2 :: InNode Int
testNode2 = Node 16 Nothing

testNode3 :: InNode Int
testNode3 = Node 20 $ Just (Node 5 Nothing)

-------------------------------------------------------------------------------

manHeaps :: (Int, Int) -> [(Int, Int)]
manHeaps (a, b) = filter isCorrectHeaps
    [ (a - 1, b    ), (a   *   2, b `div` 2)
    , (a    , b - 1), (a `div` 2, b   *   2)
    ]
  where
    isCorrectHeaps (x, y) = x >= 0 && y >= 0

zeroInMin :: (Int, Int) -> Int
zeroInMin pair = zeroInMinWithAcc [pair] 0

zeroInMinWithAcc :: [(Int, Int)] -> Int -> Int
zeroInMinWithAcc list acc = 
    if snd turnRes
    then acc + 1
    else zeroInMinWithAcc (fst turnRes) (acc + 1)
  where
    turnRes = zeroInNextTurn list

zeroInNextTurn :: [(Int, Int)] -> ([(Int, Int)], Bool)
zeroInNextTurn list = (newList, any (\(a, b) -> a == 0 && b == 0) newList)
  where
    newList = list >>= manHeaps

-------------------------------------------------------------------------------