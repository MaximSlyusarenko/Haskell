module FirstCW where

import Data.Bits ((.|.), Bits)

-- Написать функцию в две строки: тип наиболее в общем виде и реализация. Реализация должна быть максимально
-- короткой. Нельзя использовать другие пакеты, кроме Prelude, Data.List, Data.Char, Data.Map, Data.Tree.
-- Найти 2^k по заданному n, что 2^k <= n, n >= 1

findK :: Integral a => a -> a
findK n = if n < 2 then 0 else findK (div n 2) + 1

test :: Integral a => a -> Bool
test n = value <= n && 2 * value > n
  where
    value = 2 ^ findK n

testN :: Integral a => a -> String
testN 0 = "Success"
testN n = 
    if test n
    then testN $ n - 1
    else "Fail on " ++ (show $ toInteger n)

-------------------------------------------------------------------------------

-- Проверить, что переданное число содержит каждую цифру 0..9 хотя бы раз

isAllDigits :: (Bits a, Integral a) => a -> Bool
isAllDigits n = isAllDigitsWithAcc n 0

isAllDigitsWithAcc :: (Bits a, Integral a) => a -> a -> Bool
isAllDigitsWithAcc 0 acc = acc == 1023
isAllDigitsWithAcc n acc = isAllDigitsWithAcc (div n 10) ((.|.) acc (2 ^ mod n 10))

-------------------------------------------------------------------------------

-- Реализовать цикличесий список с операцией shift, возвращающий циклический список со сдвинутым на 1 элементом вправо
-- Определите оператор !!! получения значения списка по индексу

data MyList a = MyList [a]

shift :: MyList a -> MyList a
shift (MyList []) = MyList []
shift (MyList l1) = MyList $ last l1:init l1

(!!!) :: MyList a -> Int -> a
(!!!) (MyList l1) = indexWithStartList l1 l1

indexWithStartList :: [a] -> [a] -> Int -> a
indexWithStartList l1 [] n = indexWithStartList l1 l1 n
indexWithStartList _ (x:_) 0 = x
indexWithStartList l1 (_:xs) n = indexWithStartList l1 xs (n - 1)

-------------------------------------------------------------------------------

-- Реализовать свою версию Show для структуры данных из задания 3 _НЕ_ через deriving, чтобы вывод в строку был более понятным

instance Show a => Show (MyList a) where
    show (MyList []) = "->"
    show (MyList (x:xs)) = show x ++ ", " ++ show (MyList xs)

list :: MyList Int
list = MyList [1, 2, 3, 4, 5]
l :: MyList Int
l = shift list