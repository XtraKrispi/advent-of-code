{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Bifunctor (second)
import Data.Char
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)

type Row = Int
type Col = Int
type Coord = (Row, Col)
type Path = (Coord, [Coord])

type Input = Map Coord Int

part1 :: Input -> Int
part1 input = sum $ (\trailhead -> length $ nub $ fmap (\(_, p) -> last p) $ walk trailhead 0 input trailhead) <$> getAllStartingCoords input

walk :: Coord -> Int -> Input -> Coord -> [Path]
walk trailhead n m coord = do
  neighbor <- neighbors coord
  let val = Map.lookup neighbor m
  guard $ val == Just (n + 1)
  if n == 8
    then do
      pure (trailhead, [coord, neighbor])
    else do
      second (coord :) <$> walk trailhead (n + 1) m neighbor

getAllStartingCoords :: Input -> [Coord]
getAllStartingCoords = Map.keys . Map.filter (== 0)

neighbors :: Coord -> [Coord]
neighbors (row, col) =
  [ (row + 1, col)
  , (row - 1, col)
  , (row, col - 1)
  , (row, col + 1)
  ]

part2 :: Input -> Int
part2 input = sum $ (\trailhead -> length $ walk trailhead 0 input trailhead) <$> getAllStartingCoords input

makeInput :: String -> Input
makeInput contents = Map.fromList do
  (row, line) <- zip [0 ..] (lines contents)
  (col, height) <- zip [0 ..] line
  guard $ height /= '.'
  pure $ ((row, col), digitToInt height)

main :: IO ()
main =
  getArgs
    <&> listToMaybe
    <&> fromMaybe "sample.txt"
    >>= readFile
    >>= print . (part1 &&& part2) . makeInput
