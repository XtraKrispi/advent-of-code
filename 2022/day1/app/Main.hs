module Main where

import Control.Arrow ((&&&))
import Data.Foldable (maximumBy)
import Data.List (sortBy)

newtype Elf = Elf {unElf :: [Int]}
  deriving (Show)

totalCalories :: Elf -> Int
totalCalories = sum . unElf

parseInput :: String -> [Elf]
parseInput = foldr foldFn [] . lines
  where
    foldFn :: String -> [Elf] -> [Elf]
    foldFn "" [] = []
    foldFn "" elves = Elf [] : elves
    foldFn s [] = [Elf [read s]]
    foldFn s (Elf x : xs) = Elf (read s : x) : xs

day1 :: [Elf] -> Int
day1 = totalCalories . maximumBy (\e1 e2 -> compare (totalCalories e1) (totalCalories e2))

day2 :: [Elf] -> Int
day2 =
  sum
    . fmap (sum . unElf)
    . take 3
    . sortBy (\e1 e2 -> compare (totalCalories e2) (totalCalories e1))

main :: IO ()
main = do
  fileContents <- readFile "input.txt"
  print . (day1 &&& day2) $ parseInput fileContents
