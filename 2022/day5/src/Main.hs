{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Array (listArray, (!), (//))
import Data.Attoparsec.Text
  ( Parser,
    anyChar,
    decimal,
    endOfInput,
    endOfLine,
    isEndOfLine,
    many',
    parseOnly,
    string,
    takeTill,
  )
import Data.Foldable (Foldable (toList))
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.Arr (Array)
import GHC.Base (Alternative (many))
import Safe (headMay)

newtype Stack = Stack {unStack :: [Char]}
  deriving (Show)

data Move = Move
  { moveQuantity :: Int,
    moveSource :: Int,
    moveDestination :: Int
  }
  deriving (Show)

data Problem = Problem
  { stacks :: Array Int Stack,
    moves :: [Move]
  }
  deriving (Show)

moveOne :: Int -> Int -> Array Int Stack -> Array Int Stack
moveOne source destination stacks =
  let Stack sourceStack = stacks ! source
      Stack destinationStack = stacks ! destination
      newSourceStack = Stack $ drop 1 sourceStack
      newDestinationStack = maybe (Stack destinationStack) (Stack . (: destinationStack)) (headMay sourceStack)
      newStacks = stacks // [(source, newSourceStack), (destination, newDestinationStack)]
   in newStacks

moveMultiple :: Int -> Int -> Int -> Array Int Stack -> Array Int Stack
moveMultiple amt source destination stacks =
  let Stack sourceStack = stacks ! source
      Stack destinationStack = stacks ! destination
      newSourceStack = Stack $ drop amt sourceStack
      newDestinationStack = (Stack . (++ destinationStack)) (take amt sourceStack)
      newStacks = stacks // [(source, newSourceStack), (destination, newDestinationStack)]
   in newStacks

makeMovePart1 :: Array Int Stack -> Move -> Array Int Stack
makeMovePart1 stacks (Move quantity sourceStack destinationStack) =
  iterate (moveOne sourceStack destinationStack) stacks
    !! quantity

makeMovePart2 :: Array Int Stack -> Move -> Array Int Stack
makeMovePart2 stacks (Move quantity sourceStack destinationStack) =
  moveMultiple quantity sourceStack destinationStack stacks

parseCrate :: Parser (Maybe Char)
parseCrate =
  Just <$> (string "[" *> anyChar <* string "]")
    <|> Nothing <$ string "   "

parseCrateLine :: Parser [Maybe Char]
parseCrateLine =
  (:)
    <$> parseCrate
    <*> many' (void (string " ") *> parseCrate)
    <* endOfLine

parseMove :: Parser Move
parseMove = do
  void $ string "move "
  quantity <- decimal
  void $ string " from "
  source <- subtract 1 <$> decimal
  void $ string " to "
  destination <- subtract 1 <$> decimal
  pure $ Move quantity source destination

parseNumberLine :: Parser ()
parseNumberLine = do
  void $ string " "
  void $ decimal @Int
  void $ takeTill isEndOfLine
  endOfLine

parseMoves :: Parser [Move]
parseMoves = many' (parseMove <* (endOfLine <|> endOfInput))

transformCrates :: [[Maybe Char]] -> [Stack]
transformCrates = fmap (Stack . catMaybes) . transpose

parseProblem :: Parser Problem
parseProblem = do
  crates <- transformCrates <$> many parseCrateLine
  parseNumberLine
  endOfLine
  Problem (listArray (0, length crates - 1) crates) <$> parseMoves

getTop :: Array Int Stack -> [Char]
getTop =
  catMaybes . toList . fmap (headMay . unStack)

part1 :: Text -> Either String [Char]
part1 =
  fmap (\problem -> getTop $ foldl makeMovePart1 problem.stacks problem.moves)
    . parseOnly parseProblem

part2 :: Text -> Either String [Char]
part2 =
  fmap (\problem -> getTop $ foldl makeMovePart2 problem.stacks problem.moves)
    . parseOnly parseProblem

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  print $ part1 input
  print $ part2 input
