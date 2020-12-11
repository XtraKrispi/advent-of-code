{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((>=>))
import Data.List (delete, sort)

paths :: [Int] -> [[(Int, Int)]]
paths adapters = go maxAdapter 0 adapters
  where
    maxAdapter = maximum adapters + 3
    go _ _ [] = [[]]
    go max current _ | current + 3 == max = [[]]
    go max current joltages = do
      possibility <- filter (\j -> j - current < 4 && j - current >= 0) joltages
      ((possibility, possibility - current) :) <$> go max possibility (delete possibility joltages)

-- for adapter in sorted(adapters):
--     for diff in range(1, 4):
--         next_adapter = adapter + diff
--         if next_adapter in adapters:
--             paths[next_adapter] += paths[adapter]

safeMinimum :: Ord a => [a] -> Maybe a
safeMinimum a
  | null a = Nothing
  | otherwise = Just $ minimum a

part1 :: [Int] -> Integer
part1 input =
  uncurry (*)
    . foldr
      ( \(_, diff) (one, three) ->
          case diff of
            1 -> (one + 1, three)
            2 -> (one, three)
            3 -> (one, three + 1)
      )
      (0, 1)
    . head
    . filter (\l -> length l == length input)
    . paths
    . sort
    $ input

-- Not mine, I intend to rewrite using different mechanisms, but this is from SimonBaars
part2 :: [Int] -> Integer
part2 input = head $ foldr (\i acc -> sum [acc !! (j - i -1) | j <- [i + 1 .. i + 3], j < length n, (n !! j) - (n !! i) <= 3] : acc) [1] [0 .. length n -2]
  where
    n = sort $ maximum input + 3 : 0 : input

solve :: FilePath -> IO ()
solve =
  readFile >=> print . (part1 &&& part2) . (read <$>) . lines

writeSorted :: FilePath -> FilePath -> IO ()
writeSorted dest = readFile >=> writeFile dest . unlines . (show <$>) . reverse . sort . ((read <$>) :: [String] -> [Int]) . lines

main :: IO ()
main = solve "input.txt"
