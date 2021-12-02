{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Void
import Safe (headMay)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Instruction
  = Dive Int
  | Surface Int
  | Forward Int
  deriving (Show)

newtype Depth = Depth {unDepth :: Int}
newtype HorizontalPosition = HorizontalPosition {unHorizontalPosition :: Int}

type Position = (Depth, HorizontalPosition)

type Input = [Instruction]

instructionParser :: Parser Instruction
instructionParser = do
  (Dive <$> (string "down" *> char ' ' *> decimal))
    <|> (Surface <$> (string "up" *> char ' ' *> decimal))
    <|> (Forward <$> (string "forward" *> char ' ' *> decimal))

part1 :: Input -> Int
part1 =
  (\(Depth a, HorizontalPosition b) -> a * b)
    . foldl processInstruction (Depth 0, HorizontalPosition 0)
 where
  processInstruction (Depth d, h) (Dive n) = (Depth (d + n), h)
  processInstruction (Depth d, h) (Surface n) = (Depth (d - n), h)
  processInstruction (d, HorizontalPosition h) (Forward n) = (d, HorizontalPosition (h + n))

part2 :: Input -> Int
part2 = const 0

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  readFile fileName >>= print . (part1 &&& part2) . mapMaybe (parseMaybe instructionParser) . lines
