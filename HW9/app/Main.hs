module Main where

import           Control.Monad.ST   (ST, runST)
import           Data.Array.ST      (STArray, getElems, newListArray, readArray, writeArray)
import           Data.STRef         (STRef, modifySTRef, newSTRef, readSTRef)
import           Control.DeepSeq    (NFData, rnf)

import           Control.Monad       (forM_)
import           Control.Monad.ST    (ST, runST)
import           Criterion.Main      (bench, bgroup, defaultMain, nf)
import qualified Data.DList as DL    (fromList, toList, DList)
import qualified Data.List as L      (foldl')
import qualified Data.Sequence as DS (Seq, fromList, update)

data ArrayList s e = ArrayList 
    { capacity :: STRef s Int
    , size     :: STRef s Int
    , arrayRef :: STRef s (STArray s Int e)
    }

showArrayList :: ArrayList s Int -> ST s String
showArrayList arrayList = do
    array <- readSTRef $ arrayRef arrayList
    elements <- getElems array
    return $ show elements

pushBack :: ArrayList s e -> e -> ST s ()
pushBack arrayList element = do
    array <- readSTRef $ arrayRef arrayList
    arrayCapacity <- readSTRef $ capacity arrayList
    arraySize <- readSTRef $ size arrayList
    if arraySize + 1 < arrayCapacity
        then do
            writeArray array arraySize element
            modifySTRef (arrayRef arrayList) (\_ -> array)
            modifySTRef (size arrayList) (\_ -> arraySize + 1)
        else do
            elements <- getElems array
            newArray <- newListArray (0, 2 * (arraySize + 1)) elements
            writeArray newArray arraySize element
            modifySTRef (arrayRef arrayList) (\_ -> newArray)
            modifySTRef (capacity arrayList) (\_ -> 2 * (arraySize + 1))
            modifySTRef (size arrayList) (\_ -> arraySize + 1)

popBack :: ArrayList s e -> ST s (Maybe e)
popBack arrayList = do
    array <- readSTRef $ arrayRef arrayList
    arrayCapacity <- readSTRef $ capacity arrayList
    arraySize <- readSTRef $ size arrayList
    if arraySize == 0
        then return Nothing
        else do
            result <- readArray array (arraySize - 1)
            modifySTRef (size arrayList) (\x -> x - 1)
            return $ Just result

get :: ArrayList s e -> Int -> ST s (Maybe e)
get arrayList index = do
    array <- readSTRef $ arrayRef arrayList
    arraySize <- readSTRef $ size arrayList
    if index < 0 || index >= arraySize
        then return Nothing
        else do
            result <- readArray array index
            return $ Just result

set :: ArrayList s e -> Int -> e -> ST s ()
set arrayList index element = do
    array <- readSTRef $ arrayRef arrayList
    arraySize <- readSTRef $ size arrayList
    if index < 0 || index >= arraySize
        then return ()
        else do
            writeArray array index element
            modifySTRef (arrayRef arrayList) (\_ -> array)

fromList :: [e] -> ST s (ArrayList s e)
fromList elements = do
    let newCapacity = 2 * (length elements)
    let newSize = length elements
    array <- newListArray (0, newCapacity) elements
    arrayList <- newSTRef array
    arrayCapacity <- newSTRef newCapacity
    arraySize <- newSTRef newSize
    return $ ArrayList arrayCapacity arraySize arrayList

toList :: ArrayList s e -> ST s [e]
toList arrayList = do
    array <- readSTRef $ arrayRef arrayList
    elements <- getElems array
    arraySize <- readSTRef $ size arrayList
    return $ take arraySize elements

-------------------------------------------------------------------------------

testMyArrayList :: ST s (ArrayList s Int)
testMyArrayList = do
    arrayList <- fromList [1..5000]
    return arrayList

testBasicList :: [Int]
testBasicList = [1..5000]

testDList :: DL.DList Int
testDList = DL.fromList [1..5000]

testSeq :: DS.Seq Int
testSeq = DS.fromList [1..5000]

main :: IO ()
main = do
    defaultMain [ bgroup "Create"  [     bench "MyArrayList" $ nf createMyArrayListTest 5000
                                       , bench "List" $ nf createBasicList 5000
                                       , bench "DList" $ nf createDList 5000
                                       , bench "Sequence" $ nf createSequence 5000
                                    ]
                , bgroup "Set"      [     bench "MyArrayList" $ nf (\_ -> setMyArrayList [3, 400, 1000, 2500, 4500]) 100
                                        , bench "List" $ nf (\_ -> setBasicList testBasicList [3, 400, 1000, 2500, 4500]) 100
                                        , bench "DList" $ nf (\_ -> setDList [3, 400, 1000, 2500, 4500]) 100
                                        , bench "Sequence" $ nf (\_ -> setSeq testSeq [3, 400, 1000, 2500, 4500]) 100
                                    ]
                ]

createMyArrayList :: Int -> ST s (ArrayList s Int)
createMyArrayList n = do 
    arrayList <- fromList []
    forM_ [1..n] $ \x -> pushBack arrayList x
    return arrayList

createMyArrayListTest :: Int -> [Int]
createMyArrayListTest n = runST $ do
    arrayList <- createMyArrayList n
    toList arrayList

setToPositionsMyArrayList :: [Int] -> ST s (ArrayList s Int)
setToPositionsMyArrayList [] = testMyArrayList
setToPositionsMyArrayList (x:xs) = do
    arrayList <- testMyArrayList
    set arrayList x 1
    setToPositionsMyArrayList xs

setMyArrayList :: [Int] -> [Int]
setMyArrayList positions = runST $ do
    setToPositionsMyArrayList positions
    arrayList <- testMyArrayList
    toList arrayList

createBasicList :: Int -> [Int]
createBasicList n = L.foldl' (\list element -> list ++ [element]) [] [1..n]

setBasicList :: [Int] -> [Int] -> [Int]
setBasicList currentList [] = currentList
setBasicList currentList (x:xs) = setBasicList (replaceBasicList x 1 [] currentList) xs

replaceBasicList :: Int -> Int -> [Int] -> [Int] -> [Int]
replaceBasicList 0 element prevList (x:postList) = prevList ++ [element] ++ postList
replaceBasicList position element prevList (x:postList) = 
    replaceBasicList (position - 1) element (prevList ++ [x]) postList

createDList :: Int -> [Int]
createDList n = DL.toList $ DL.fromList [1..n]

setDList :: [Int] -> DL.DList Int
setDList positions = DL.fromList $ setBasicList (DL.toList testDList) positions

createSequence :: Int -> DS.Seq Int
createSequence n = DS.fromList [1..n]

setSeq :: DS.Seq Int -> [Int] -> DS.Seq Int
setSeq currentSeq [] = currentSeq
setSeq currentSeq (x:xs) = setSeq (DS.update x 1 currentSeq) xs