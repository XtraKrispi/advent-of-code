module Main where

import Control.Arrow
import Debug.Trace

type Input = [Int]

part1 :: Input -> Int
part1 (x : y : xs) = if y > x then 1 + part1 (y : xs) else part1 (y : xs)
part1 _ = 0

part2 :: Input -> Int
part2 = part1 . chunked []

chunked :: Input -> Input -> Input
chunked accum (x : y : z : xs) = chunked (accum ++ [x + y + z]) (y : z : xs)
chunked accum _ = accum

main :: IO ()
main = do
  readFile "input.txt" >>= print . (part1 &&& part2) . fmap read . lines
