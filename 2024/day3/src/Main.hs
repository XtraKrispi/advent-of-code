module Main where

import Control.Arrow ((&&&))

type Input = ()

part1 :: Input -> Int
part1 input = 0

part2 :: Input -> Int
part2 input = 0

makeInput :: String -> Input
makeInput contents = ()

main :: IO ()
main = do
  input <- makeInput <$> readFile "sample.txt"
  print $ (part1 &&& part2) input
