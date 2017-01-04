module Main where

import           System.Environment        (getArgs)
import qualified Data.Map as M             (Map, toList, insert, lookup, empty)
import qualified Data.Text as T 
import           Data.Text.IO              (getLine, hGetContents, putStrLn)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.State       (StateT, get, modify, runStateT)
import           Control.Monad.Trans.Class (lift)
import           Prelude hiding            (getLine, putStr, putStrLn)
import           System.IO                 (Handle (..), IOMode (..), openFile)
import           Data.Maybe                (fromMaybe)

main :: IO ()
main = do
    (fileName:other) <- getArgs
    let fName = T.pack fileName
    putStrLn $ T.pack "Interactive options:"
    putStrLn $ T.pack "   * B <value> : Modify previous value"
    putStrLn $ T.pack "   * W         : Finish session and write to file"
    putStrLn $ T.pack "   * A         : Abort session (discard all changes)"
    handle <- openFile fileName ReadWriteMode
    fcontent <- hGetContents handle
    (_, props)<- runStateT (runInteractive fileName) (parseInitialMap fcontent)
    return ()

runInteractive :: FilePath -> StateT (M.Map T.Text T.Text) IO ()
runInteractive fileName = do
    liftIO $ putStrLn (T.pack "> Input property and value: ")
    command <- liftIO getLine
    if command == T.pack "W" then
        do
            resultMap <- get
            liftIO $ writeToFile fileName resultMap
    else if command == T.pack "A" then
        return ()
    else if T.isPrefixOf (T.pack "B ") command then
        do
            currentMap <- get
            let propText = T.drop 2 command
            let propValue = fromMaybe (T.pack "Nothing") (M.lookup propText currentMap)
            liftIO $ putStrLn (T.pack "Input new value for `" `T.append` propText
                `T.append` T.pack "` property (previous: `" `T.append` propValue `T.append` T.pack "`)")
            newValue <- liftIO getLine
            modify (M.insert propText newValue)
            runInteractive fileName
    else
        do
            let pair = getKeyValuePair command
            modify (M.insert (fst pair) (snd pair))
            runInteractive fileName

writeToFile :: FilePath -> M.Map T.Text T.Text -> IO ()
writeToFile fileName result = do
    let list = M.toList result
    putStrLn $ T.pack ("Next properties are written in `" ++ fileName ++ "`")
    mapM_ print $ map (\(a, b) -> a `T.append` T.pack "=" `T.append` b) list
    writeFile fileName $ T.unpack (T.intercalate (T.pack "\n") (map (\(a, b) -> 
        a `T.append` T.pack "=" `T.append` b) list) `T.append` T.pack "\n")

getKeyValuePair :: T.Text -> (T.Text, T.Text)
getKeyValuePair x = 
    let (key:value) = T.splitOn (T.pack "=") x
    in (key, head value)


parseInitialMap :: T.Text -> M.Map T.Text T.Text
parseInitialMap contents = let firstSplit = T.lines contents
    in getInitialMap firstSplit M.empty

getInitialMap :: [T.Text] -> M.Map T.Text T.Text -> M.Map T.Text T.Text
getInitialMap [] current = current
getInitialMap (x:xs) current = let pair = getKeyValuePair x
    in getInitialMap xs (M.insert (fst pair) (snd pair) current)