module Parser where

import           Data.Char (isDigit, isUpper, isSpace, isAlpha, isAlphaNum)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

first :: (a -> b) -> Maybe (a, c) -> Maybe (b, c)
first f Nothing = Nothing
first f (Just (x, y)) = Just $ (f x, y)

instance Functor Parser where
    fmap f parser = Parser (\s -> first f (runParser parser s))

instance Applicative Parser where
    pure x = Parser (\s -> Just (x, s))
    (Parser f1) <*> (Parser f2) = Parser (\s -> (f1 s) 
        >>= (\(func, restStr1) -> (f2 restStr1) 
            >>= (\(val, restStr2) -> return $ (func val, restStr2))))

oneSymbolParser :: Char -> Parser Char
oneSymbolParser c = Parser (\s ->
    case s of
        []     -> Nothing 
        (x:xs) ->
            if x == c 
            then Just (c, xs)
            else Nothing)

abParser :: Parser (Char, Char)
abParser = (,) <$> (oneSymbolParser 'a') <*> (oneSymbolParser 'b')

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> (oneSymbolParser 'a') <*> (oneSymbolParser 'b')

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
        | null ns   = Nothing
        | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs

intPair :: Parser [Integer]
intPair = ((:) <$> posInt) <*> ((\_ x -> [x]) <$> (oneSymbolParser ' ') <*> posInt)

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

instance Alternative Parser where
    empty = Parser (\_ -> Nothing)
    (Parser f1) <|> (Parser f2) = 
        Parser (\s -> case (f1 s) of
            Nothing -> f2 s
            Just x  -> Just x)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs) 
        | p x       = Just (x, xs)
        | otherwise = Nothing

upperCaseParser :: Parser ()
upperCaseParser = (\_ -> ()) <$> satisfy isUpper

posInt_ :: Parser ()
posInt_ = (\_ -> ()) <$> posInt

intOrUppercase :: Parser ()
intOrUppercase = posInt_ <|> upperCaseParser

-------------------------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser = Parser (\s -> return $ fromMaybeSafe (createList [] parser s) s)

createList :: [a] -> Parser a -> String -> Maybe ([a], String)
createList prev parser@(Parser f) curString = 
    (f curString) >>= (\(val, rest) -> 
        case createList (prev ++ [val]) parser rest of
            Nothing -> Just (prev ++ [val], rest)
            Just x  -> Just x)

fromMaybeSafe :: (Maybe ([a], String)) -> String -> ([a], String)
fromMaybeSafe Nothing s = ([], s)
fromMaybeSafe (Just t) _       = t

oneOrMore :: Parser a -> Parser [a]
oneOrMore parser = Parser (\s -> createList [] parser s)

-------------------------------------------------------------------------------

spaceParser :: Parser Char
spaceParser = satisfy isSpace

spaces :: Parser String
spaces = zeroOrMore spaceParser

alphaNumParser :: Parser Char
alphaNumParser = satisfy isAlphaNum

alphaParser :: Parser Char
alphaParser = satisfy isAlpha

identParser :: Parser String
identParser = (\c d -> [c] ++ d) <$> alphaParser <*> (zeroOrMore alphaNumParser)

ident :: Parser String
ident = (\[s] -> s) <$> oneOrMore identParser

-------------------------------------------------------------------------------

type Ident = String

data Atom = N Integer | I Ident deriving Show

data SExpr = A Atom
           | Comb [SExpr]
           deriving Show

--(*>) :: Applicative f => f a -> f b -> f b
--p1 *> p2 = (\_ x -> x) <$> p1 <*> p2 

--(<*) :: Applicative f => f a -> f b -> f a
--p1 <* p2 = (\x _ -> x) <$> p1 <*> p2

parseSExpr :: Parser SExpr
parseSExpr = Comb <$> (spaces *> (oneOrMore ((A <$> ((N <$> posInt) <|> (I <$> ident))) <|> 
    (oneSymbolParser '(' *> (Comb <$> oneOrMore parseSExpr) <* oneSymbolParser ')'))) <* spaces)