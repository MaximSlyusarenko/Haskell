module MyArrayList where

import           Control.Monad.ST (ST, runST)
import           Data.Array.ST    (STArray, getElems, newListArray, readArray, writeArray)
import           Data.STRef       (STRef, modifySTRef, newSTRef, readSTRef)

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

test :: ST s (ArrayList s Int)
test = do
    arrayList <- fromList [2, 8, 12]
    set arrayList 2 1
    pushBack arrayList 7
    popBack arrayList
    pushBack arrayList 16
    popBack arrayList
    popBack arrayList
    return arrayList

pushBackToTest :: Int -> ST Int ()
pushBackToTest element = do
    arrayList <- test
    pushBack arrayList element

getList :: [Int]
getList = runST $ do
    arrayList <- test
    toList arrayList

getByIndex :: Int -> Maybe Int
getByIndex index = runST $ do
    arrayList <- test
    get arrayList index