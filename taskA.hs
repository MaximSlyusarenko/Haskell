module TaskA where

import Data.Char

mul10 :: Int -> Int
mul10 x = 10 * x

addOrSub :: Int -> Int -> (Int -> Int -> Int) -> Int
addOrSub x y f = f x y

checkWasNegativeOrUnaryPlusOrLeadingZero :: Bool -> Bool -> Bool -> Bool
checkWasNegativeOrUnaryPlusOrLeadingZero x y z = 
    if x || y || z
    then True
    else False

isCorrectNumber :: Int -> Bool -> Bool -> Bool -> Bool
isCorrectNumber curNumber negative wasUnaryPlus wasLeadingZero = 
    if (negative || wasUnaryPlus) && curNumber == 0
    then wasLeadingZero
    else True



stringSumWithAcc :: String -> Bool -> Bool -> Bool -> Int -> Int -> Int
stringSumWithAcc [] negative wasUnaryPlus wasLeadingZero currentNumber acc = 
    if isCorrectNumber currentNumber negative wasUnaryPlus wasLeadingZero
    then 
        if negative
        then addOrSub acc currentNumber (-)
        else addOrSub acc currentNumber (+)
    else
        error "Incorrect number"
stringSumWithAcc (x:xs) negative wasUnaryPlus wasLeadingZero currentNumber acc = 
    if isSpace x
    then
        if isCorrectNumber currentNumber negative wasUnaryPlus wasLeadingZero
        then
            if negative
            then stringSumWithAcc xs False False False 0 (addOrSub acc currentNumber (-))
            else stringSumWithAcc xs False False False 0 (addOrSub acc currentNumber (+))
        else error "Incorrect number"
    else
        if x == '-' || x == '+'
        then 
            if checkWasNegativeOrUnaryPlusOrLeadingZero negative wasUnaryPlus wasLeadingZero
            then error "Incorrect number"
            else
                if currentNumber /= 0
                then error "Incorrect number"                                                    
                else
                    if x == '-' 
                    then stringSumWithAcc xs True wasUnaryPlus wasLeadingZero currentNumber acc
                    else stringSumWithAcc xs negative True wasLeadingZero currentNumber acc
        else
            if currentNumber == 0
            then
                if x == '0'
                then stringSumWithAcc xs negative wasUnaryPlus True currentNumber acc
                else stringSumWithAcc xs negative wasUnaryPlus wasLeadingZero (digitToInt x) acc
            else
                if isDigit x
                then stringSumWithAcc xs negative wasUnaryPlus wasLeadingZero (mul10 currentNumber + digitToInt x) acc
                else error "Incorrect number"

stringSum :: String -> Int
stringSum s = stringSumWithAcc s False False False 0 0

stringSum' :: String -> Int
stringSum' s = sum $ map (\t -> read t :: Int) $ words s