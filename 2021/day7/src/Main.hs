module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Safe (headMay)
import System.Environment (getArgs)

type Input = [Int]

solve :: (Input -> Int -> (Int, Int)) -> Input -> Maybe (Int, Int)
solve calcFn input =
  let min = minimum input
      max = maximum input
   in foldr
        ( \i mins ->
            let item@(num, fuel) = calcFn input i
             in case mins of
                  Nothing -> Just item
                  Just (_, fuel') | fuel < fuel' -> Just item
                  _ -> mins
        )
        Nothing
        [min .. max]

part1 :: Input -> Int
part1 = maybe 0 snd . solve calc

calc :: Input -> Int -> (Int, Int)
calc input n = (n, foldr (\i fuel -> abs (i - n) + fuel) 0 input)

calcWithStep :: Input -> Int -> (Int, Int)
calcWithStep input n = (n, foldr (\i fuel -> fuelCost (abs (i - n)) + fuel) 0 input)
 where
  fuelCost diff =
    sum [0 .. diff]

part2 :: Input -> Int
part2 = maybe 0 snd . solve calcWithStep

transform :: String -> Input
transform = (read <$>) . splitOn ","

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  readFile fileName >>= print . (part1 &&& part2) . transform
