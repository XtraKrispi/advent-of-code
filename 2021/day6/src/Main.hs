module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Safe (headMay)
import System.Environment (getArgs)

type Input = [Int]

--part1 :: Input -> Int
part1 input = length $ foldr (\_ i -> singleDay i) input [0 .. 79]

part2 :: Input -> Int
part2 = const 0

singleDay :: Input -> [Int]
singleDay = uncurry (++) . foldr foldFn ([], [])
 where
  foldFn 0 (existing, new) = (6 : existing, 8 : new)
  foldFn n (existing, new) = (n - 1 : existing, new)

transform :: String -> Input
transform = (read <$>) . splitOn ","

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  readFile fileName >>= print . (part1 &&& part2) . transform
