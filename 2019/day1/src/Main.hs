module Main where

import Control.Arrow

part1 :: Integer -> Integer
part1 n = (n `div` 3) - 2

part2 :: Integer -> Integer
part2 n = go n 0

go n' total
  | n' <= 0 = total
  | otherwise =
    let res = max 0 (part1 n')
     in go res (total + res)

main :: IO ()
main = do
  readFile "input.txt"
    >>= print . ((sum . (part1 <$>)) &&& (sum . (part2 <$>))) . fmap read . lines
