module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad (guard)
import Data.Maybe (fromMaybe, listToMaybe)

part1 :: [Int] -> Int
part1 xs = maybe 0 (uncurry (*)) (listToMaybe go)
  where
    go = do
      x <- xs
      y <- xs

      guard $ x + y == 2020

      return (x, y)

part2 :: [Int] -> Int
part2 xs = maybe 0 (\(a, b, c) -> a * b * c) (listToMaybe go)
  where
    go = do
      x <- xs
      y <- xs
      z <- xs
      guard $ x + y + z == 2020

      return (x, y, z)

main :: IO ()
main = do
  nums <- fmap read . lines <$> readFile "input.txt"

  print $ (part1 &&& part2) nums
