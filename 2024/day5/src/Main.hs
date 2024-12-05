module Main where

import Control.Arrow ((&&&))

type Input = ()

part1 :: Input -> Int
part1 _ = 0

part2 :: Input -> Int
part2 _ = 0

makeInput :: String -> Input
makeInput _ = ()

main :: IO ()
main = makeInput <$> readFile "sample.txt" >>= print . (part1 &&& part2)
