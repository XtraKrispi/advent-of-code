module Main where

import Control.Arrow ((&&&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, listToMaybe)
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
    <&> listToMaybe
    <&> fromMaybe "sample.txt"
    >>= readFile
    >>= print . (part1 &&& part2) . makeInput
