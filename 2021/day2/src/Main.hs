{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Void (Void)
import Safe (headMay)
import System.Environment (getArgs)
import Text.Megaparsec (Parsec, parseMaybe, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

tripleToPair :: (a, b, c) -> (a, b)
tripleToPair (a, b, _) = (a, b)

type Parser = Parsec Void String

data Instruction
  = Dive Int
  | Surface Int
  | Forward Int
  deriving (Show)

newtype Depth = Depth {unDepth :: Int}
newtype HorizontalPosition = HorizontalPosition {unHorizontalPosition :: Int}
newtype Aim = Aim {unAim :: Int}
type Position = (Depth, HorizontalPosition)

type Input = [Instruction]

instructionParser :: Parser Instruction
instructionParser = do
  (Dive <$> (string "down" *> char ' ' *> decimal))
    <|> (Surface <$> (string "up" *> char ' ' *> decimal))
    <|> (Forward <$> (string "forward" *> char ' ' *> decimal))

part1 :: Input -> Int
part1 =
  uncurry (*)
    . bimap unDepth unHorizontalPosition
    . foldl processInstruction (Depth 0, HorizontalPosition 0)
 where
  processInstruction (Depth d, h) (Dive n) = (Depth (d + n), h)
  processInstruction (Depth d, h) (Surface n) = (Depth (d - n), h)
  processInstruction (d, HorizontalPosition h) (Forward n) = (d, HorizontalPosition (h + n))

part2 :: Input -> Int
part2 =
  uncurry (*)
    . bimap unDepth unHorizontalPosition
    . tripleToPair
    . foldl processInstruction (Depth 0, HorizontalPosition 0, Aim 0)
 where
  processInstruction (d, h, Aim a) (Dive n) = (d, h, Aim (a + n))
  processInstruction (d, h, Aim a) (Surface n) = (d, h, Aim (a - n))
  processInstruction (Depth d, HorizontalPosition h, Aim a) (Forward n) =
    (Depth (d + (a * n)), HorizontalPosition (h + n), Aim a)

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  readFile fileName >>= print . (part1 &&& part2) . mapMaybe (parseMaybe instructionParser) . lines
