module Main where

import Control.Arrow ((&&&))
import Data.IntMap qualified as IntMap
import Data.List (sort)
import Data.Maybe (fromMaybe)

type Input = ([Int], [Int])

part1 :: Input -> Int
part1 (xs, ys) = sum $ (\(x, y) -> abs (y - x)) <$> zip (sort xs) (sort ys)

part2 :: Input -> Int
part2 (xs, ys) = foldr (\x s -> s + x * (fromMaybe 0 (IntMap.lookup x yMap))) 0 xs
 where
  yMap :: IntMap.IntMap Int = foldr (\y -> IntMap.insertWith (+) y 1) IntMap.empty ys

makeInput :: String -> Input
makeInput =
  foldr
    ( \a (xs, ys) ->
        case words a of
          [x, y] -> (read x : xs, read y : ys)
          _ -> (xs, ys)
    )
    ([], [])
    . lines

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let input = makeInput contents
  print $ (part1 &&& part2) input
