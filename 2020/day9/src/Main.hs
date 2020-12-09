module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((>=>))

isValid :: Int -> [Int] -> Bool
isValid n preamble = or [x + y == n | x <- preamble, y <- preamble, x /= y]

part1 :: Int -> [Int] -> Int
part1 preambleLength nums =
  go (nums !! preambleLength) (take preambleLength nums) (drop (preambleLength + 1) nums)
  where
    go n preamble nums'
      | isValid n preamble = go (head nums') (tail preamble ++ [n]) (tail nums')
      | otherwise = n

getSumValues :: Int -> [Int] -> [Int]
getSumValues total nums =
  if null results
    then getSumValues total (tail nums)
    else results
  where
    results = go nums []
    go [] accum = accum
    go (num : nums') accum
      | sum accum + num == total = num : accum
      | sum accum + num > total = []
      | otherwise = go nums' (num : accum)

part2 :: Int -> [Int] -> Int
part2 preambleLength nums =
  let invalid = part1 preambleLength nums
   in uncurry (+)
        . (minimum &&& maximum)
        . getSumValues invalid
        $ nums

solve :: Int -> FilePath -> IO ()
solve preambleLength =
  readFile
    >=> print
      . (part1 preambleLength &&& part2 preambleLength)
      . (read <$>)
      . lines

main :: IO ()
main = solve 25 "input.txt"
