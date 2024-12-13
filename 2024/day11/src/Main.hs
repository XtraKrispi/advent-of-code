module Main where

import Control.Arrow ((&&&))
import Data.Functor ((<&>))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Maybe (fromMaybe, listToMaybe)
import Data.MemoTrie (memo2)
import System.Environment (getArgs)

type Stone = Int
type Cycle = Int

type Input = [Stone]

blink :: [Stone] -> [Stone]
blink input = input >>= applyRule

solve :: Int -> Input -> Int
solve n input = sum (apply blinks n (IntMap.fromListWith (+) [(i, 1) | i <- input]))

blinks :: IntMap Int -> IntMap Int
blinks stones = IntMap.fromListWith (+) [(stone', n) | (stone, n) <- IntMap.assocs stones, stone' <- applyRule stone]

solve' :: Int -> Input -> Int
solve' n = sum . map (descendantCount n)

descendantCount :: Int -> Int -> Int
descendantCount = memo2 go
 where
  go 0 _ = 1
  go iters n = sum $ descendantCount (pred iters) <$> applyRule n

applyRule :: Stone -> [Stone]
applyRule 0 = [1]
applyRule x
  | even (length (show x)) =
      let str = show x
          (left, right) = splitAt (length str `div` 2) str
       in [read left, read right]
  | otherwise = [x * 2024]

apply :: (a -> a) -> Int -> a -> a
apply _ 0 val = val
apply fn n val = apply fn (n - 1) (fn val)

part1 :: Input -> Int
part1 = solve' 25

part2 :: Input -> Int
part2 = solve' 75

makeInput :: String -> Input
makeInput = (read <$>) . words

main :: IO ()
main =
  getArgs
    <&> listToMaybe
    <&> fromMaybe "sample.txt"
    >>= readFile
    >>= print . (part1 &&& part2) . makeInput
