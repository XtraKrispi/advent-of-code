{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative (Alternative (many, (<|>)))
import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((>=>))
import Data.Attoparsec.ByteString (Parser, endOfInput, parseOnly, string)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import Data.Either (rights)
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Debug.Trace (trace)

data InfixOp = Add | Multiply
  deriving (Show)

data Expr
  = Digit Int
  | Binary Expr InfixOp Expr
  deriving (Show)

applyOp :: InfixOp -> (Int -> Int -> Int)
applyOp Add = (+)
applyOp Multiply = (*)

eval :: Expr -> Int
eval (Digit i) = i
eval (Binary expr op expr') = applyOp op (eval expr) (eval expr')

expressionParser :: Parser Expr
expressionParser = do
  left <- unaryParser
  right <- many ((,) <$> opParser <*> unaryParser)
  pure $ foldl' (\expr (op, expr') -> Binary expr op expr') left right

unaryParser :: Parser Expr
unaryParser = parenParser <|> digitParser

opParser :: Parser InfixOp
opParser = (Add <$ string "+") <|> (Multiply <$ string "*")

digitParser :: Parser Expr
digitParser = Digit <$> decimal

parenParser :: Parser Expr
parenParser = do
  string "("
  expr <- expressionParser
  string ")"
  pure expr

-- | Part 2 stuff is here
data TermData = Number Int | Operator InfixOp | Parened [Term]
  deriving (Show)

newtype Term = Term
  {unTerm :: (TermData, Bool)}
  deriving
    (Show)

formString :: [Term] -> String
formString = foldr (\t str -> char t <> str) "" . (fst . unTerm <$>)
  where
    char t = case t of
      Number i -> show i
      Operator Add -> "+"
      Operator Multiply -> "*"
      Parened terms -> "(" <> formString terms <> ")"

termsParser :: Parser [Term]
termsParser = do
  parsed <- many $ (Number <$> decimal) <|> parens <|> (Operator Multiply <$ string "*") <|> (Operator Add <$ string "+")
  pure $ Term . (,False) <$> parsed
  where
    parens = do
      string "("
      terms <- termsParser
      string ")"
      pure $ Parened terms

evalTerm :: Term -> Term
evalTerm (Term (Parened terms, False)) = Term (Parened (injectParens terms), True)
evalTerm (Term (t, False)) = Term (t, True)
evalTerm t = t

injectParens :: [Term] -> [Term]
injectParens [] = []
injectParens (term : Term (Operator Add, _) : term' : rest) = injectParens $ Term (Parened [evalTerm term, (Term (Operator Add, True)), evalTerm term'], True) : rest
injectParens (term : rest) = evalTerm term : injectParens rest

convert :: ByteString -> Either String Expr
convert = parseOnly expressionParser . B.concat . C.words

part1 :: [ByteString] -> Int
part1 = sum . (eval <$>) . rights . (convert <$>)

part2 :: [ByteString] -> Int
part2 =
  sum
    . (eval <$>)
    . rights
    . (convert <$>)
    . rights
    . (fmap (C.pack . formString . injectParens) . parseOnly termsParser . B.concat . C.words <$>)

solve :: FilePath -> IO ()
solve = C.readFile >=> print . (part1 &&& part2) . C.lines

main :: IO ()
main = solve "input.txt"
