module Main where

import Control.Arrow ((&&&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)

type Stone = Int

type Input = [Int]

blink :: Input -> Input
blink input = input >>= applyRule
 where
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
part1 input = length $ apply blink 25 input

part2 :: Input -> Int
part2 _ = 0

makeInput :: String -> Input
makeInput = (read <$>) . words

main :: IO ()
main =
  getArgs
    <&> listToMaybe
    <&> fromMaybe "sample.txt"
    >>= readFile
    >>= print . (part1 &&& part2) . makeInput
