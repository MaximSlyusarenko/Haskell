module Main where

import           System.Environment (getArgs)
import           System.Directory (doesFileExist)
import qualified Data.Map as M (Map, toList, insert, lookup, empty)
import           Data.List.Split (splitOn)
import           Data.List (isPrefixOf, intercalate)
import           Data.IORef (IORef, atomicModifyIORef, readIORef, newIORef)
import           System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE funcs #-}
funcs :: IORef (M.Map String String)
funcs = unsafePerformIO $ newIORef M.empty

main :: IO ()
main = do
    (fileName:other) <- getArgs
    contents <- getFileContents fileName
    putStrLn "Interactive options:"
    putStrLn "   * B <value> : Modify previous value"
    putStrLn "   * W         : Finish session and write to file"
    putStrLn "   * A         : Abort session (discard all changes)"
    parseInitialMap contents
    runInteractive fileName

getFileContents :: FilePath -> IO String
getFileContents fileName = do
    exists <- doesFileExist fileName
    if exists then
        readFile fileName
    else
        return $ ""

runInteractive :: String -> IO ()
runInteractive fileName = do
    initialMap <- readIORef funcs
    putStrLn "> Input property and value: "
    command <- getLine
    if command == "W" then
        writeToFile fileName
    else if command == "A" then
        return $ ()
    else if (isPrefixOf "B " command) then
        updateMap (drop 2 command) fileName
    else
        executeInsertCommand command fileName

writeToFile :: String -> IO ()
writeToFile fileName = do
    result <- readIORef funcs
    let list = M.toList result
    putStrLn $ "Next properties are written in `" ++ fileName ++ "`"
    mapM_ print $ map (\(a, b) -> a ++ "=" ++ b) list
    writeFile fileName $ (intercalate "\n" (map (\(a, b) -> a ++ "=" ++ b) list)) ++ "\n"

updateMap :: String -> String -> IO ()
updateMap property fileName = do
    value <- getPropertyValue property
    putStrLn ("Input new value for `" ++ property ++ "` property (previous: `" ++ 
        value ++ "`)")
    value <- getLine
    atomicModifyIORef funcs (\m -> (M.insert property value m, ()))
    runInteractive fileName

getPropertyValue :: String -> IO String
getPropertyValue property = do
    m <- readIORef funcs
    case M.lookup property m of
        Just value -> return $ value
        Nothing -> return $ "Nothing"

getKeyValuePair :: String -> (String, String)
getKeyValuePair x = 
    let (key:value) = splitOn "=" x
    in (key, head value)

executeInsertCommand :: String -> String -> IO ()
executeInsertCommand command fileName = do
    let pair = getKeyValuePair command
    atomicModifyIORef funcs (\m -> (M.insert (fst pair) (snd pair) m, ()))
    runInteractive fileName

parseInitialMap :: String -> IO ()
parseInitialMap contents = do
    let firstSplit = init $ splitOn "\n" contents
    getInitialMap firstSplit

getInitialMap :: [String] -> IO ()
getInitialMap [] = return $ ()
getInitialMap ("":[]) = return $ ()
getInitialMap (x:xs) = do 
    let pair = getKeyValuePair x
    atomicModifyIORef funcs (\m -> (M.insert (fst pair) (snd pair) m, ()))
    getInitialMap xs