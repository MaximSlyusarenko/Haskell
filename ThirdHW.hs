module ThirdHW where

import qualified TreePrinters as T
       ( Tree (..) )

import SecondHW 
       ( insert
       , find
       , delete
       , findMin )

newtype Coin color = Coin 
    { 
        getCoin :: Int 
    } deriving (Show)

data Blue
data Red

class ComparableColor a where
    getColorNumber :: Coin a -> Int

instance ComparableColor Blue where
    getColorNumber _ = 0

instance ComparableColor Red where
    getColorNumber _ = 1

instance Num (Coin color) where
    (+) a b = Coin (getCoin a + getCoin b)
    (*) a b = Coin (getCoin a * getCoin b)
    abs a = Coin (abs $ getCoin a)
    signum a = Coin (signum $ getCoin a)
    fromInteger a = Coin (fromInteger a)
    negate a = Coin (negate $ getCoin a)

instance Monoid (Coin color) where
    mempty = Coin 0
    mappend a b = a + b

compareCoins :: (ComparableColor c1, ComparableColor c2) => Coin c1 -> Coin c2 -> Ordering
compareCoins c1 c2
    | getColorNumber c1 < getColorNumber c2  = LT
    | getColorNumber c1 > getColorNumber c2  = GT
    | getColorNumber c1 == getColorNumber c2 = compare (getCoin c1) (getCoin c2)

-------------------------------------------------------------------------------

instance Ord a => Monoid (T.Tree a) where
    mempty = T.Leaf
    mappend a T.Leaf = a
    mappend T.Leaf a = a
    mappend tree (T.Node element left right) = mappend (mappend (insert element tree) left) right

instance Foldable T.Tree where
    foldr _ z T.Leaf = z
    foldr f z (T.Node element left right) = foldr f (f element (foldr f z right)) left

-------------------------------------------------------------------------------

class Set t where
    emptySet :: Ord a => t a
    toList :: Ord a => t a -> [a]
    findSet :: Ord a => a -> t a -> Maybe a
    insertSet :: Ord a => a -> t a -> t a
    deleteSet :: Ord a => a -> t a -> t a
    next :: Ord a => a -> t a -> Maybe a
    fromList :: Ord a => [a] -> t a

instance Set T.Tree where
    emptySet = mempty
    toList = foldr (:) []
    findSet = find
    insertSet = insert
    deleteSet = delete
    next = findNext T.Leaf
    fromList = foldr insertSet T.Leaf

findNext :: Ord a => T.Tree a -> a -> T.Tree a -> Maybe a
findNext _ _ T.Leaf = Nothing
findNext rightParent x curNode@(T.Node element left right)
    | x > element  = findNext rightParent x right
    | x < element  = findNext curNode x left
    | x == element = getNextWithDifferentRight rightParent right

getNextWithDifferentRight :: Ord a => T.Tree a -> T.Tree a -> Maybe a  -- If there isn't right parent then rightParent is Leaf
getNextWithDifferentRight T.Leaf T.Leaf = Nothing
getNextWithDifferentRight (T.Node element _ _) T.Leaf = Just element
getNextWithDifferentRight _ rightNode = getNext rightNode

getNext :: Ord a => T.Tree a -> Maybe a
getNext T.Leaf = Nothing
getNext curNode = Just $ findMin curNode


-------------------------------------------------------------------------------

newtype Pair a b = Pair (a, b) deriving Show

instance Eq a => Eq (Pair a b) where
    (==) (Pair a) (Pair b) = fst a == fst b

instance Ord a => Ord (Pair a b) where
    (<=) (Pair a) (Pair b) = fst a <= fst b

class Map t where
    emptyMap :: Ord a => t (Pair a b)
    toListMap :: Ord a => t (Pair a b) -> [(a, b)]
    findMap :: Ord a => a -> t (Pair a b) -> Maybe b
    insertMap :: Ord a => (a, b) -> t (Pair a b) -> t (Pair a b)
    deleteMap :: Ord a => a -> t (Pair a b) -> t (Pair a b)
    nextMap :: Ord a => a -> t (Pair a b) -> Maybe (a, b)
    fromListMap :: Ord a => [(a, b)] -> t (Pair a b)

instance Map T.Tree where
    emptyMap = emptySet
    toListMap = map convertToStandardPair . toList
    findMap x tree =
        case findSet (Pair (x, undefined)) tree of
            Just (Pair (_, b)) -> Just b
            Nothing         -> Nothing
    insertMap x = insertSet (Pair x)
    deleteMap x = deleteSet (Pair (x, undefined))
    nextMap x tree = 
        case next (Pair (x, undefined)) tree of
            Just res  -> Just $ convertToStandardPair res
            Nothing -> Nothing
    fromListMap = fromList . map Pair

convertToStandardPair :: Pair a b -> (a, b)
convertToStandardPair (Pair x) = x

y :: T.Tree (Pair Int Int)
y = emptyMap :: T.Tree (Pair Int Int)

t :: T.Tree (Pair Int Int)
t = insertMap (10, 20) y

z :: T.Tree (Pair Int Int)
z = fromListMap [(1, 4), (3, 5), (1, 2), (1, 3), (4, 8), (11, 0)]