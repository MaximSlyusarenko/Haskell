module Main where

import System.Random            (StdGen, randomR, mkStdGen)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent       (threadDelay)
import Control.Concurrent.MVar  (MVar, newEmptyMVar, putMVar, takeMVar)

main :: IO ()
main = do
    mv <- newEmptyMVar
    magicians <- initTournament participants
    putMVar mv magicians
    putStrLn $ "Tournament is ready, participants are: " ++ (show magicians)
    winner <- tournament mv magicians
    putStrLn $ "Winner is " ++ (show winner)

data Magician = Magician [StdGen] Int Int Int Int Int | Dead -- Attack, Defence, Speed, Instability, Index

instance Show Magician where
    show Dead = "Dead"
    show (Magician _ _ defence _ _ _) = "Magician (health = " ++ (show defence) ++ ")"

participants :: Int
participants = 3

generateParticipant :: Int -> IO Magician
generateParticipant number = return $ Magician [gen] attack defence speed instability number
  where
    firstGen = mkStdGen number
    (attack, secondGen) = randomR (1, 100) firstGen
    (defence, thirdGen) = randomR (1, 200) secondGen
    (speed, fourthGen) = randomR (1, 20) thirdGen
    (instability, gen) = randomR (10 ^ 7, 2 * (10 ^ 7)) fourthGen 

initTournament :: Int -> IO [Magician]
initTournament numberOfParticipants = mapConcurrently generateParticipant [1..numberOfParticipants]

roundForOneMagician :: MVar [Magician] -> Magician -> IO (Maybe Int)
roundForOneMagician mv Dead = return Nothing
roundForOneMagician mv currentMagician@(Magician generators attack defence speed instability currentIndex) = do
            putStrLn $ "Round for magician with index: " ++ (show currentIndex)
            let gen = head generators
            let (rival, secondGen) = randomR (1, participants) gen
            putStrLn $ "Magician " ++ (show currentIndex) ++ " wants to attack magician " ++ (show rival)
            if rival == currentIndex then do
                let generators3 = secondGen:(tail generators)
                oldMagicians <- takeMVar mv
                let changedMagicians = changeGeneratorsForMagician oldMagicians currentIndex generators3 []
                let newCurrentMagician2 = getMagicianByIndex changedMagicians currentIndex
                putMVar mv changedMagicians
                roundForOneMagician mv newCurrentMagician2
            else do
                let generators1 = secondGen:(tail generators)
                attackValuesAndGenerators <- mapConcurrently (generateAttackValue instability) (map (\x -> (x, attack)) generators1)
                let generators2 = map snd attackValuesAndGenerators
                let attackValues = map fst attackValuesAndGenerators
                let attackValue = foldr max 0 attackValues
                putStrLn $ "Magician " ++ (show currentIndex) ++ " wants to attack magician " ++ (show rival) ++ " with value " ++ (show attackValue)
                threadDelay (speed * 10 ^ 6)
                putStrLn $ "Magician " ++ (show currentIndex) ++ " wants to get the lock"
                previousMagicians <- takeMVar mv
                putStrLn $ "Magician " ++ (show currentIndex) ++ " gets lock" 
                let currentDead = isDead previousMagicians currentIndex
                putStrLn $ "Is current magician (index = " ++ (show currentIndex) ++ ") dead? " ++ (show currentDead)
                let rivalDead = isDead previousMagicians rival
                if currentDead then do
                    putMVar mv previousMagicians
                    putStrLn $ "Magician " ++ (show currentIndex) ++ " releases lock" 
                    roundForOneMagician mv Dead
                else
                    if rivalDead then
                        do
                            let magicians = changeGeneratorsForMagician previousMagicians currentIndex generators2 []
                            let newCurrentMagician = getMagicianByIndex magicians currentIndex
                            putMVar mv magicians
                            putStrLn $ "Magician " ++ (show currentIndex) ++ " releases lock" 
                            if (checkOnlyCurrentAlive previousMagicians) then
                                return $ Just currentIndex
                            else
                                roundForOneMagician mv newCurrentMagician
                    else
                        do
                            let magicians = changeGeneratorsForMagician previousMagicians currentIndex generators2 []
                            let newMagiciansAndGens = subtractFromRival magicians rival attackValue ([], [])
                            let newMagicians1 = fst newMagiciansAndGens
                            let gensToAdd = snd newMagiciansAndGens
                            let newMagicians = addGeneratorsForMagician newMagicians1 currentIndex gensToAdd []
                            let newCurrentMagician1 = getMagicianByIndex newMagicians currentIndex
                            putStrLn $ "New magicians: " ++ (show newMagicians)
                            putMVar mv newMagicians
                            putStrLn $ "Magician " ++ (show currentIndex) ++ " releases lock" 
                            if (checkOnlyCurrentAlive newMagicians) then
                                return $ Just currentIndex
                            else
                                roundForOneMagician mv newCurrentMagician1

isDead :: [Magician] -> Int -> Bool
isDead [] _ = True
isDead (Dead:xs) index = isDead xs index
isDead ((Magician _ _ _ _ _ ind):xs) index = 
    if ind == index then
        False
    else
        isDead xs index

checkOnlyCurrentAlive :: [Magician] -> Bool
checkOnlyCurrentAlive magicians = 
    (foldr (\magician -> case magician of 
        Dead -> (+) 0
        _ -> (+) 1) 0 magicians) == 1

subtractFromRival :: [Magician] -> Int -> Int -> ([Magician], [StdGen]) -> ([Magician], [StdGen])
subtractFromRival [] _ _ currentResult = currentResult
subtractFromRival (Dead:xs) magicianIndex attackValue currentResult = 
    subtractFromRival xs magicianIndex attackValue ((addToList (fst currentResult) Dead), snd currentResult)
subtractFromRival (magician@(Magician gens attack defence speed instability ind):xs) magicianIndex attackValue currentResult = 
    if ind == magicianIndex then
        if defence - attackValue < 0 then
            subtractFromRival xs magicianIndex attackValue ((addToList (fst currentResult) Dead), gens)
        else
            subtractFromRival xs magicianIndex attackValue 
                (addToList (fst currentResult) (Magician gens attack (defence - attackValue) speed instability ind), [])
    else
        subtractFromRival xs magicianIndex attackValue (addToList (fst currentResult) magician, snd currentResult)

changeGeneratorsForMagician :: [Magician] -> Int -> [StdGen] -> [Magician] -> [Magician]
changeGeneratorsForMagician [] _ _ currentResult = currentResult
changeGeneratorsForMagician (Dead:xs) magicianIndex newGenerators currentResult = 
    changeGeneratorsForMagician xs magicianIndex newGenerators (addToList currentResult Dead)
changeGeneratorsForMagician (magician@(Magician gens attack defence speed instability ind):xs) magicianIndex newGenerators currentResult =
    if ind == magicianIndex then
        changeGeneratorsForMagician xs magicianIndex newGenerators (addToList currentResult 
            (Magician newGenerators attack defence speed instability ind))
    else
        changeGeneratorsForMagician xs magicianIndex newGenerators (addToList currentResult magician)

addGeneratorsForMagician :: [Magician] -> Int -> [StdGen] -> [Magician] -> [Magician]
addGeneratorsForMagician [] _ _ currentResult = currentResult
addGeneratorsForMagician (Dead:xs) magicianIndex newGenerators currentResult = 
    addGeneratorsForMagician xs magicianIndex newGenerators (addToList currentResult Dead)
addGeneratorsForMagician (magician@(Magician gens attack defence speed instability ind):xs) magicianIndex newGenerators currentResult =
    if ind == magicianIndex then
        addGeneratorsForMagician xs magicianIndex newGenerators (addToList currentResult 
            (Magician (gens ++ newGenerators) attack defence speed instability ind))
    else
        addGeneratorsForMagician xs magicianIndex newGenerators (addToList currentResult magician)

getMagicianByIndex :: [Magician] -> Int -> Magician
getMagicianByIndex [] index = Dead
getMagicianByIndex (Dead:xs) index = getMagicianByIndex xs index
getMagicianByIndex (magician@(Magician _ _ _ _ _ ind):xs) index = 
    if (ind == index) then
        magician
    else
        getMagicianByIndex xs index

addToList :: [a] -> a -> [a]
addToList list element = list ++ [element]

tournament :: MVar [Magician] -> [Magician] -> IO (Int)
tournament mv participants = do
    result <- mapConcurrently (\participant -> roundForOneMagician mv participant) participants
    let winner = findJust result
    return winner

findJust :: [Maybe a] -> a
findJust (Nothing:xs) = findJust xs
findJust ((Just x):xs) = x

generateManyTimes :: Int -> (Int, StdGen) -> Int -> (Int, StdGen)
generateManyTimes _ currentResult 0 = currentResult
generateManyTimes attack (_, gen) x = generateManyTimes attack (randomR (1, attack) gen) (x - 1)

generateAttackValue :: Int -> (StdGen, Int) -> IO (Int, StdGen)
generateAttackValue instability (gen, attack) = return $ generateManyTimes attack (0, gen) instability