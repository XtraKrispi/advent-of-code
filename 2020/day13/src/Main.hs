{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((>=>))
import Data.Bifunctor (Bifunctor (second))
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Safe (readMay)

part1 :: (Integer, [Maybe Integer]) -> Integer
part1 (earliest, mBuses) =
  (\(busId, time) -> busId * (time - earliest))
    . minimumBy (\(_, a) (_, b) -> compare a b)
    . concat
    . ( take 1 . dropWhile (\(_, y) -> y < earliest) . (\bus -> (bus,) <$> busTimes bus)
          <$>
      )
    $ catMaybes mBuses

busTimes :: Integer -> [Integer]
busTimes busId = (* busId) <$> [0 ..]

part2Naive :: [Maybe Integer] -> Integer
part2Naive = check 100000000000000 . concat . (flipIt <$>) . zip [0 ..]

flipIt :: (Integer, Maybe Integer) -> [(Integer, Integer)]
flipIt (i, Nothing) = []
flipIt (i, Just busId) = [(i, busId)]

check :: Integer -> [(Integer, Integer)] -> Integer
check val [] = 0
check val total@((i, initial) : rest) =
  if all (validation val) total
    then val
    else check (val + initial) total

validation :: Integer -> (Integer, Integer) -> Bool
validation val (i, busId) = (val + i) `mod` busId == 0

part2 :: [Maybe Integer] -> Integer
part2 buses =
  case concat $ flipIt <$> zip [0 ..] buses of
    ((idx, busId) : rest) ->
      go (busId, busId) rest
  where
    go (t, _) [] = t
    go (t, step) ((idx, busId) : rest) =
      go (increaseTime t idx busId step, lcm step busId) rest
    increaseTime t idx busId step
      | (t + idx) `mod` busId == 0 = t
      | otherwise = increaseTime (t + step) idx busId step

convert :: [String] -> (Integer, [Maybe Integer])
convert [earliest, buses] = (read earliest, (readMay <$>) $ splitOn "," buses)

solve :: FilePath -> IO ()
solve = readFile >=> print . (part1 &&& (part2 . snd)) . convert . lines

main :: IO ()
main = solve "input.txt"
