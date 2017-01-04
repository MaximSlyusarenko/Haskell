module SevenSecond where

import           Control.Applicative       (many, some, liftA, liftA2)
import           Control.Monad             (join, void)
import           Control.Monad.Reader      (Reader, asks, local, runReader)
import qualified Data.Map                  as M
import           Data.Monoid               ((<>))
import           Options.Applicative       (command, execParser, fullDesc, help, helper,
                                            info, long, metavar, progDesc, short,
                                            strOption, subparser)
import           Options.Applicative.Types (Parser (..))
import           Text.Megaparsec           (alphaNumChar, between, letterChar, parse,
                                            parseTest, (<?>), (<|>))
import           Text.Megaparsec.Char      (spaceChar)
import           Text.Megaparsec.Expr      (Operator (..), makeExprParser)
import qualified Text.Megaparsec.Lexer     as L
import qualified Text.Megaparsec.String    as MP

data Mode = PrintAst
          | Eval (M.Map String Int)

data Options = Options String Mode deriving (Show, Read)

evalMode :: Parser Mode
evalMode = 

parseCommand :: Parser (IO ())
parseCommand = subparser $ 
    command "print-ast" (info (runPrintAst <$> (helper <*> startExpression <*> printAstMode)) fullDesc)
    <> command "eval" (info (runEval <$> (helper <*> startExpression <*> evalMode)) fullDesc)

data Expr   = Lit Int
            | Var String
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Pow Expr Expr
            | Neg Expr
            | Id Expr
            deriving Show

eval :: Expr -> Reader (M.Map String Int) (Maybe Int)
eval (Lit i) = return $ Just i
eval (Var v) = asks $ M.lookup v
eval (Neg e) = liftA negate <$> eval e
eval (Id e) = eval e
eval (Add e1 e2) = eval e1 >>= \r1 ->
                   eval e2 >>= \r2 ->
                   return $ liftA2 (+) r1 r2
eval (Mul e1 e2) = eval e1 >>= \r1 ->
                   eval e2 >>= \r2 ->
                   return $ liftA2 (*) r1 r2
eval (Sub e1 e2) = eval e1 >>= \r1 ->
                   eval e2 >>= \r2 ->
                   return $ liftA2 (-) r1 r2
eval (Div e1 e2) = eval e1 >>= \r1 ->
                   eval e2 >>= \r2 ->
                   return $ liftA2 div r1 r2
eval (Pow e1 e2) = eval e1 >>= \r1 ->
                   eval e2 >>= \r2 ->
                   return $ liftA2 (^) r1 r2