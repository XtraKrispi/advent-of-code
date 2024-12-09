module Main where

import Control.Arrow ((&&&))
import Data.Functor ((<&>))
import Data.Maybe
import System.Environment (getArgs)

type Input = ()

part1 :: Input -> Int
part1 _ = 0

part2 :: Input -> Int
part2 _ = 0

makeInput :: String -> Input
makeInput _ = ()

main :: IO ()
main =
  getArgs
    <&> fromMaybe "sample.txt" . listToMaybe
    >>= readFile
    >>= print . (part1 &&& part2) . makeInput
