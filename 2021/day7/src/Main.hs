module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Safe (headMay)
import System.Environment (getArgs)

type Input = [Int]

part1 :: Input -> Maybe (Int, Int)
part1 input =
  let min = minimum input
      max = maximum input
   in foldr
        ( \i mins ->
            let item@(num, fuel) = calc input i
             in case mins of
                  Nothing -> Just item
                  Just (_, fuel') | fuel < fuel' -> Just item
                  _ -> mins
        )
        Nothing
        [min .. max]

calc :: Input -> Int -> (Int, Int)
calc input n = (n, foldr (\i fuel -> abs (i - n) + fuel) 0 input)

part2 :: Input -> Int
part2 = const 0

transform :: String -> Input
transform = (read <$>) . splitOn ","

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  readFile fileName >>= print . (part1 &&& part2) . transform
