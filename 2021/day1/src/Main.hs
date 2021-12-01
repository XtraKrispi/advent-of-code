module Main where

import Control.Arrow
import Debug.Trace

type Input = [Int]

part1 :: Input -> Int
part1 =
  snd
    . foldl
      ( \(prev, num) i -> case prev of
          Just p | i > p -> (Just i, num + 1)
          _ -> (Just i, num)
      )
      (Nothing, 0)

part2 :: Input -> Int
part2 = part1 . chunked []

chunked :: Input -> Input -> Input
chunked accum [] = accum
chunked accum [x] = accum
chunked accum [x, y] = accum
chunked accum (x : y : z : xs) = chunked (accum ++ [x + y + z]) (y : z : xs)

main :: IO ()
main = do
  readFile "input.txt" >>= print . (part1 &&& part2) . fmap read . lines
