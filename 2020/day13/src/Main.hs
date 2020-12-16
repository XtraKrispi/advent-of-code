{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((>=>))
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Safe (readMay)

part1 :: (Int, [Maybe Int]) -> Int
part1 (earliest, mBuses) = (\(busId, time) -> busId * (time - earliest)) $ minimumBy (\(_, a) (_, b) -> compare a b) $ concat $ fmap (take 1 . dropWhile (\(_, y) -> y < earliest)) $ (\bus -> (bus,) <$> busTimes bus) <$> catMaybes mBuses

busTimes :: Int -> [Int]
busTimes busId = (* busId) <$> [0 ..]

part2 :: (Int, [Maybe Int]) -> Int
part2 = const 0

convert :: [String] -> (Int, [Maybe Int])
convert [earliest, buses] = (read earliest, (readMay <$>) $ splitOn "," buses)

solve :: FilePath -> IO ()
solve = readFile >=> print . (part1 &&& part2) . convert . lines

main :: IO ()
main = do
  putStrLn "hello world"
