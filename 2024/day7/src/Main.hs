{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Arrow ((&&&))
import Data.Either (rights)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, spaceChar)
import Text.Megaparsec.Char.Lexer (decimal)

data Equation = Equation
  { total :: Int
  , numbers :: [Int]
  }
  deriving (Show)

equationParser :: Parsec Void String Equation
equationParser =
  Equation
    <$> (decimal <* char ':')
    <*> (many (spaceChar *> decimal))

type Input = [Equation]

data Operator = Multiplication | Addition | Concatenation
  deriving (Show)

type Result = (Bool, [Operator])

possibleResults :: [Operator] -> Equation -> [Result]
possibleResults ops (Equation total numbers) = go numbers []
 where
  go (x : y : xs) accum = do
    operator <- ops
    case operator of
      Multiplication -> go (x * y : xs) (accum ++ [Multiplication])
      Addition -> go (x + y : xs) (accum ++ [Addition])
      Concatenation -> go (read (show x ++ show y) : xs) (accum ++ [Concatenation])
  go [x] accum | x == total = pure (True, accum)
  go _ accum = pure (False, accum)

part1 :: Input -> Int
part1 = computeResults [Multiplication, Addition]

part2 :: Input -> Int
part2 =
  computeResults [Multiplication, Addition, Concatenation]

computeResults :: [Operator] -> Input -> Int
computeResults ops =
  sum
    . fmap total
    . filter
      ( any (\x -> fst x)
          . possibleResults ops
      )

makeInput :: String -> Input
makeInput contents = rights $ (\line -> parse equationParser line line) <$> lines contents

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . makeInput
