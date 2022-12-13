{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.Text
  ( Parser,
    decimal,
    endOfInput,
    endOfLine,
    many',
    parseOnly,
    string,
  )
import Data.Either (fromRight)
import Data.List (intersect)
import Data.Text qualified as T
import Data.Text.IO qualified as T

newtype Elf = Elf {unElf :: [Int]}
  deriving (Show)

parseFile :: Parser a -> Parser [a]
parseFile p = many' (p <* (endOfLine <|> endOfInput))

parseElf :: Parser Elf
parseElf = (\d1 d2 -> Elf [d1 .. d2]) <$> decimal <* string "-" <*> decimal

parsePair :: Parser (Elf, Elf)
parsePair = (,) <$> parseElf <* string "," <*> parseElf

allEncompassing :: (Elf, Elf) -> Bool
allEncompassing (Elf e1, Elf e2)
  | intersect e1 e2 == e1 = True
  | intersect e1 e2 == e2 = True
  | otherwise = False

overlapAtAll :: (Elf, Elf) -> Bool
overlapAtAll (Elf e1, Elf e2)
  | null (e1 `intersect` e2) = False
  | otherwise = True

part :: ((Elf, Elf) -> Bool) -> T.Text -> Int
part fn =
  length
    . filter fn
    . fromRight []
    . parseOnly (parseFile parsePair)

part1 :: T.Text -> Int
part1 = part allEncompassing

part2 :: T.Text -> Int
part2 = part overlapAtAll

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  print $ part1 input
  print $ part2 input