module Main where
    
main :: IO ()
main = putStrLn "Running tests" >>
        putStrLn ("Test 1: " ++ "[1, 3, 6], expected: " 
            ++ (show $ simpleSafeHead [1, 3, 6]) ++ ", actual: " ++ (show $ safeHead [1, 3, 6])) >>
        putStrLn ("Test 2: " ++ "[] :: [Bool], expected: " 
            ++ (show $ simpleSafeHead ([] :: [Bool])) ++ ", actual: " ++ (show $ safeHead ([] :: [Bool]))) >>
        putStrLn ("Test 3: " ++ "[7], expected: " 
            ++ (show $ simpleSafeHead [7]) ++ ", actual: " ++ (show $ safeHead [7])) >>
        putStrLn ("Test 4: " ++ "[7, 5, 8], expected: " 
            ++ (show $ simpleSafeHead [7, 5, 8]) ++ ", actual: " ++ (show $ safeHead [7, 5, 8])) >>
        putStrLn ("Test 5: " ++ "[\"a\", \"b\", \"c\"], expected: " 
            ++ (show $ simpleSafeHead ["a", "b", "c"]) ++ ", actual: " ++ (show $ safeHead ["a", "b", "c"])) >>
        putStrLn ("Test 6: " ++ "\"abc\", expected: " 
            ++ (show $ simpleSafeHead "abc") ++ ", actual: " ++ (show $ safeHead "abc"))

-- Реализуйте функцию safeHead, используя свертку

safeHead :: (Show a) => [a] -> Maybe a
safeHead = foldr (\x _ -> Just x) Nothing

simpleSafeHead :: (Show a) => [a] -> Maybe a
simpleSafeHead [] = Nothing
simpleSafeHead list = Just $ head list
