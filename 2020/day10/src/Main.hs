{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((>=>))
import Data.List (delete, sort)

safeMinimum :: Ord a => [a] -> Maybe a
safeMinimum a
  | null a = Nothing
  | otherwise = Just $ minimum a

getPath :: (Num b, Ord b) => [b] -> [(b, b)]
getPath joltages =
  (\lst -> (0, minimum (snd <$> lst)) : lst) $
    head $
      filter (\track -> length track == length joltages) $ getPaths joltages

getPaths :: (Num b, Ord b) => [b] -> [[(b, b)]]
getPaths joltages = go joltages adapter [[]]
  where
    adapter = 3 + maximum joltages
    go _ 0 accum = accum
    go [] _ accum = accum
    go js target (latest : rest) =
      let possibilities = filter (\j -> target - j < 4 && target - j >= 0) js
       in do
            possibility <- possibilities -- trace ("target: " <> show target <> " possibilities: " <> show possibilities) possibilities
            go (delete possibility js) (target - (target - possibility)) (((possibility, target - possibility) : latest) : rest)

part1 :: [Int] -> Int
part1 =
  uncurry (*)
    . foldr
      ( \(_, diff) (one, three) ->
          case diff of
            1 -> (one + 1, three)
            2 -> (one, three)
            3 -> (one, three + 1)
      )
      (0, 0)
    . getPath
    . reverse
    . sort

part2 :: [Int] -> Int
part2 = const 0

solve :: FilePath -> IO ()
solve =
  readFile >=> print . (part1 &&& part2) . (read <$>) . lines

main :: IO ()
main = solve "input.txt"
