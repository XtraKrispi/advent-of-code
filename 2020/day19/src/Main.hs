{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((>=>))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace (trace)
import Safe (headMay)
import Text.Megaparsec (MonadParsec (eof), Parsec, many, parse, parseMaybe)
import Text.Megaparsec.Char (char, letterChar, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec String String

type Message = String

data Rule = SingleChar Char | SubRules [Int] [Int]
  deriving (Show)

type Rules = IntMap Rule

data DataFile = DataFile [Message] Rules
  deriving (Show)

part1 :: DataFile -> Int
part1 (DataFile messages rules) = length $ filter (validate rules 0) messages

part2 :: DataFile -> Int
part2 = const 0

combine :: [[a]] -> [[a]] -> [[a]]
combine [] yss = yss
combine xss yss = do
  xs <- xss
  ys <- yss
  pure $ xs ++ ys

extract :: Rules -> Int -> [String]
extract rules ruleNum =
  case Map.lookup ruleNum rules of
    Just (SubRules left right) ->
      let lefts =
            foldl
              (\r l -> combine r (extract rules l))
              []
              left
          rights =
            foldl
              (\r l -> combine r (extract rules l))
              []
              right
       in lefts ++ rights
    Just (SingleChar c) -> pure [c]
    Nothing -> trace "here" []

validate :: Rules -> Int -> String -> Bool
validate rules ruleNum input =
  let allValid = extract rules ruleNum
   in input `elem` allValid

intListParser :: Parser [Int]
intListParser = many (decimal <* (() <$ char ' ' <|> eof))

charParser :: Parser Rule
charParser = SingleChar <$> (char '\"' *> letterChar <* char '\"')

subRulesParser :: Parser Rule
subRulesParser = do
  first <- intListParser
  SubRules first <$> (([] <$ eof) <|> orParser)
  where
    orParser = do
      string "| "
      intListParser

rulesParser :: Parser (Int, Rule)
rulesParser = do
  id <- decimal
  string ": "
  (id,) <$> (charParser <|> subRulesParser)

convert :: [String] -> Maybe DataFile
convert lines =
  let rules = takeWhile (/= "") lines
      messages = drop 1 $ dropWhile (/= "") lines
   in DataFile messages . Map.fromList <$> sequence (parseMaybe rulesParser <$> rules)

solve :: FilePath -> IO ()
solve = readFile >=> print . ((part1 &&& part2) <$>) . convert . lines

main :: IO ()
main = solve "input.txt"
