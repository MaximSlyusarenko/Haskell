module SecondHW where

import TreePrinters
       ( Tree
       , Tree(Node)
       , Tree(Leaf)
       , verticalPrint)

safeTail :: [a] -> Either String [a]
safeTail [] = Left "The List is empty"
safeTail (_:xs) = Right xs

safeInit :: [a] -> Either String [a]
safeInit [] = Left "The List is empty"
safeInit (_:[]) = Right []
safeInit (x:xs) = case safeInit xs of
    Left s -> Left s
    Right l -> Right (x:l)

strip :: [a] -> [a]
strip l = case safeTail l of
    Left _ -> []
    Right list -> case safeInit list of
        Left _ -> []
        Right ans -> ans

-------------------------------------------------------------------------------

data Player 
    = Player Int Int Int Equipment Equipment 
    | DeadPlayer 
    deriving (Show)  -- Health, attack, defence

data Equipment = Attack Int 
               | Defence Int 
               | HealthPotion 
               | NoEquipment 
               deriving (Show)

data Monster = Monster Int Int Int Equipment Equipment Equipment 
             | DeadMonster 
             deriving (Show)  -- Health, attack, defence

data Result = Result Int Player 
            | Cicle String 
            deriving (Show)  -- Number of killing monsters and player at the end

class Unit a where
    getHealth :: a -> Int
    getAttack :: a -> Int
    getDefence :: a -> Int
    getEquipmentAttackInt :: a -> Int
    getEquipmentAttack :: a -> Equipment
    getEquipmentDefenceInt :: a -> Int
    getEquipmentDefence :: a -> Equipment
    hasHealthPotion :: a -> Bool
    getHealthPotion :: a -> Equipment
    getFullAttack :: a -> Int
    getFullDefence :: a -> Int
    healthEquals :: a -> a -> Bool

instance Unit Player where
    getHealth (Player health _ _ _ _) = health
    getHealth DeadPlayer = 0
    getAttack (Player _ attack _ _ _) = attack
    getAttack DeadPlayer = 0
    getDefence (Player _ _ defence _ _) = defence
    getDefence DeadPlayer = 0
    getEquipmentAttackInt (Player _ _ _ (Attack x) _) = x
    getEquipmentAttackInt (Player _ _ _ NoEquipment _) = 0
    getEquipmentAttackInt DeadPlayer = 0
    getEquipmentAttack (Player _ _ _ x _) = x
    getEquipmentAttack DeadPlayer = NoEquipment
    getEquipmentDefenceInt (Player _ _ _ _ (Defence x)) = x
    getEquipmentDefenceInt (Player _ _ _ _ NoEquipment) = 0
    getEquipmentDefenceInt DeadPlayer = 0
    getEquipmentDefence (Player _ _ _ _ x) = x
    getEquipmentDefence DeadPlayer = NoEquipment
    hasHealthPotion _ = False
    getHealthPotion _ = NoEquipment
    getFullAttack player = getAttack player + getEquipmentAttackInt player
    getFullDefence player = getDefence player + getEquipmentDefenceInt player
    healthEquals player1 player2 = getHealth player1 == getHealth player2

instance Unit Monster where
    getHealth (Monster health _ _ _ _ _) = health
    getHealth DeadMonster = 0
    getAttack (Monster _ attack _ _ _ _) = attack
    getAttack DeadMonster = 0
    getDefence (Monster _ _ defence _ _ _) = defence
    getDefence DeadMonster = 0
    getEquipmentAttackInt (Monster _ _ _ (Attack x) _ _) = x
    getEquipmentAttackInt (Monster _ _ _ NoEquipment _ _) = 0
    getEquipmentAttackInt DeadMonster = 0
    getEquipmentAttack (Monster _ _ _ x _ _) = x
    getEquipmentAttack DeadMonster = NoEquipment
    getEquipmentDefenceInt (Monster _ _ _ _ (Defence x) _) = x
    getEquipmentDefenceInt (Monster _ _ _ _ NoEquipment _) = 0
    getEquipmentDefenceInt DeadMonster = 0
    getEquipmentDefence (Monster _ _ _ _ x _) = x
    getEquipmentDefence DeadMonster = NoEquipment
    hasHealthPotion (Monster _ _ _ _ _ HealthPotion) = True
    hasHealthPotion (Monster _ _ _ _ _ NoEquipment) = False
    hasHealthPotion DeadMonster = False
    getHealthPotion (Monster _ _ _ _ _ x) = x
    getHealthPotion DeadMonster = NoEquipment
    getFullAttack monster = getAttack monster + getEquipmentAttackInt monster
    getFullDefence monster = getDefence monster + getEquipmentDefenceInt monster
    healthEquals monster1 monster2 = getHealth monster1 == getHealth monster2

startPlayer :: Player
startPlayer = Player 100 10 10 NoEquipment NoEquipment

monstersSample :: [Monster]
monstersSample = Monster 100 1 1 (Attack 10) NoEquipment NoEquipment:
    Monster 100 11 1 NoEquipment (Defence 10) NoEquipment:
    Monster 100 9 11 (Attack 10) (Defence 10) NoEquipment:[]

maraudAttackEquipment :: Player -> Monster -> Player
maraudAttackEquipment player monster
    | getEquipmentAttackInt monster > getEquipmentAttackInt player = 
        Player (getHealth player) (getAttack player) (getDefence player) 
        (getEquipmentAttack monster) (getEquipmentDefence player)
    | otherwise = player

maraudDefenceEquipment :: Player -> Monster -> Player
maraudDefenceEquipment player monster
    | getEquipmentDefenceInt monster > getEquipmentDefenceInt player = 
        Player (getHealth player) (getAttack player) (getDefence player) 
        (getEquipmentAttack player) (getEquipmentDefence monster)
    | otherwise = player

maraudHealthPotion :: Player -> Monster -> Player
maraudHealthPotion player monster =
    if hasHealthPotion monster
    then Player 100 (getAttack player) (getDefence player) 
    (getEquipmentAttack player) (getEquipmentDefence player)
    else player

maraudMonster :: Player -> Monster -> Player
maraudMonster player monster = maraudHealthPotion 
    (maraudDefenceEquipment (maraudAttackEquipment player monster) monster) monster

gloriousBattleWithLevelCounter :: Player -> [Monster] -> Int -> Result
gloriousBattleWithLevelCounter player (monster:monsters) level = 
    case roundResult of
        (DeadPlayer, Monster _ _ _ _ _ _) -> Result level DeadPlayer
        (fightPlayer@(Player _ _ _ _ _), DeadMonster) -> 
            gloriousBattleWithLevelCounter (maraudMonster fightPlayer monster) monsters (level + 1)
        (fightPlayer@(Player _ _ _ _ _), fightMonster@(Monster _ _ _ _ _ _)) -> 
            if healthEquals player fightPlayer && healthEquals monster fightMonster
            then Cicle ("Player can't kill monster " ++ (show $ level + 1) ++ 
                ", monster " ++ (show $ level + 1) ++ " can't kill player")
            else gloriousBattleWithLevelCounter fightPlayer (fightMonster:monsters) level
  where 
    roundResult = fightRound player monster
gloriousBattleWithLevelCounter player [] level = Result level player

gloriousBattle :: Player -> [Monster] -> Result
gloriousBattle player monsters = gloriousBattleWithLevelCounter player monsters 0

fightRound :: Player -> Monster -> (Player, Monster)
fightRound player monster = monsterTurn (fst x) (snd x) 
  where 
    x = playerTurn player monster

playerTurn :: Player -> Monster -> (Player, Monster)
playerTurn player monster = 
    if getFullAttack player > getFullDefence monster
    then if getHealth monster - getFullAttack player + getFullDefence monster <= 0
        then (player, DeadMonster)
        else (player, Monster (getHealth monster - getFullAttack player + getFullDefence monster) 
            (getAttack monster) (getDefence monster) (getEquipmentAttack monster) 
            (getEquipmentDefence monster) (getHealthPotion monster))
    else (player, monster)

monsterTurn :: Player -> Monster -> (Player, Monster)
monsterTurn player monster = 
    if getFullAttack monster > getFullDefence player
    then if getHealth player - getFullAttack monster + getFullDefence player <= 0
        then (DeadPlayer, monster)
        else (Player (getHealth player - getFullAttack monster + getFullDefence player) 
            (getAttack player) (getDefence player) (getEquipmentAttack player) 
            (getEquipmentDefence player), monster)
    else (player, monster)

-------------------------------------------------------------------------------

--data Tree a = Node a (Tree a) (Tree a) | Leaf

find :: (Ord a) => a -> Tree a -> Maybe a
find x (Node element left right)
    | element == x      = Just element
    | element > x       = find x left
    | element < x       = find x right
find _ Leaf = Nothing

insert :: (Ord a) => a -> Tree a -> Tree a
insert x curNode@(Node element left right)
    | element == x      = Node x left right
    | element > x       = Node element (insert x left) right
    | element < x       = Node element left (insert x right)
insert x Leaf = Node x Leaf Leaf

delete :: (Ord a) => a -> Tree a -> Tree a
delete x (Node element left right)
    | element == x      = case left of 
                                   Leaf -> right 
                                   Node _ _ _ -> Node (findMax left) (deleteMax left) right -- Trick for not call findMax Leaf, because it is undefined
    | element > x       = Node element (delete x left) right
    | element < x       = Node element left (delete x right)
delete _ Leaf = Leaf

toList :: (Ord a) => Tree a -> [a]
toList tree@(Node _ _ _) = (findMin tree):(toList $ deleteMin tree) 
toList Leaf = []

fromList :: (Ord a) => [a] -> Tree a
fromList (x:xs) = insert x (fromList xs)
fromList [] = Leaf

findMax :: (Ord a) => Tree a -> a
findMax (Node _ _ right@(Node _ _ _)) = findMax right
findMax (Node element _ Leaf) = element

findMin :: (Ord a) => Tree a -> a
findMin (Node _ left@(Node _ _ _) _) = findMin left
findMin (Node element Leaf _) = element

deleteMax :: (Ord a) => Tree a -> Tree a
deleteMax (Node element left right@(Node _ _ _)) = Node element left $ deleteMax right
deleteMax (Node _ left Leaf) = left
deleteMax Leaf = Leaf

deleteMin :: (Ord a) => Tree a -> Tree a
deleteMin (Node element left@(Node _ _ _) right) = Node element (deleteMin left) right
deleteMin (Node _ Leaf right) = right
deleteMin Leaf = Leaf

-------------------------------------------------------------------------------

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
    firstSplit = snd (split tree (beginIndex - 1))
    secondSplit = fst (split firstSplit (endIndex - beginIndex + 1))