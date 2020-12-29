{-# LANGUAGE OverloadedStrings #-}

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

{-
(Binary (Binary (Digit 2) Multiply (Digit 3)) Add (Binary (Digit 4) Multiply (Digit 5))
2*3+(4*5)
-}

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

part1 :: [ByteString] -> Int
part1 = sum . (eval <$>) . convert

part2 :: [ByteString] -> Int
part2 = const 0

convert :: [ByteString] -> [Expr]
convert = rights . (parseOnly expressionParser . B.concat . C.words <$>)

solve :: FilePath -> IO ()
solve = C.readFile >=> print . (part1 &&& part2) . C.lines

main :: IO ()
main = solve "input.txt"
