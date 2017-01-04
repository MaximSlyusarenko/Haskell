module SixthHW where

import           Control.Monad.Writer (Writer, writer, tell, execWriter, runWriter)
import           Control.Monad.State (State, state, execState, evalState, get)
import           TreePrinters (Tree, Tree(Node), Tree(Leaf), verticalPrint)

find :: (Show a, Ord a) => a -> Tree a -> Writer String (Maybe a)
find x (Node element left right)
    | element == x      = tell "Element was found\n" >> writer (Just element, "")
    | element > x       = tell ("Current element is " ++ (show element) ++ ", going left\n") >> 
        find x left >>= \b -> return b
    | element < x       = tell ("Current element is " ++ (show element) ++ ", going right\n") >> 
        find x left >>= \b -> return b
find _ Leaf = tell "Element was not found\n" >> writer (Nothing, "")

insert :: (Show a, Ord a) => a -> Tree a -> Writer String (Tree a)
insert x curNode@(Node element left right)
    | element == x      = tell "Element was found\n" >> writer (Node x left right, "")
    | element > x       = tell ("Current element is " ++ (show element) ++ ", going left\n") >>
        resultLeftWriter
    | element < x       = tell ("Current element is " ++ (show element) ++ ", going right\n") >>
        resultRightWriter
  where
    leftWriter = insert x left
    resultLeftWriter = writer (Node element (fst $ runWriter leftWriter) right, execWriter leftWriter)
    rightWriter = insert x right
    resultRightWriter = writer (Node element left (fst $ runWriter rightWriter), execWriter rightWriter)
insert x Leaf = writer (Node x Leaf Leaf, "Inserted\n")

delete :: (Show a, Ord a) => a -> Tree a -> Writer String (Tree a)
delete x tree@(Node element left right)
    | element == x      = tell "Element was found\n" >> getWriterAfterDelete x tree
    | element > x       = tell ("Current element is " ++ (show element) ++ ", going left\n") >>
        resultLeftWriter
    | element < x       = tell ("Current element is " ++ (show element) ++ ", going right\n") >>
        resultRightWriter
  where
    leftWriter = delete x left
    resultLeftWriter = writer (Node element (fst $ runWriter leftWriter) right, execWriter leftWriter)
    rightWriter = delete x right
    resultRightWriter = writer (Node element left (fst $ runWriter rightWriter), execWriter rightWriter)
delete _ Leaf = writer (Leaf, "Element was not found\n")

toList :: (Show a, Ord a) => Tree a -> Writer String [a]
toList tree@(Node _ _ _) = writer ((findMin tree):listBefore, 
    "Adding element " ++ (show $ findMin tree) ++ "\n" ++ logBefore)
  where
    listBeforeWriter = toList $ deleteMin tree
    listBefore = fst $ runWriter listBeforeWriter
    logBefore = execWriter listBeforeWriter
toList Leaf = writer ([], "All elements were added\n")

fromList :: (Show a, Ord a) => [a] -> Writer String (Tree a)
fromList (x:xs) = tell ("Inserting " ++ (show x) ++ "\n") >> insert x treeBefore >> treeBeforeWriter
  where
    treeBeforeWriter = fromList xs
    treeBefore = fst $ runWriter treeBeforeWriter
    logBefore = execWriter treeBeforeWriter
fromList [] = writer (Leaf, "All elements were inserted\n")

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

getWriterAfterDelete :: (Ord a) => a -> Tree a -> Writer String (Tree a)
getWriterAfterDelete x (Node element left right) = case left of 
                                   Leaf -> writer (right, "") 
                                   Node _ _ _ -> writer (Node (findMax left) (deleteMax left) right, "")

-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
