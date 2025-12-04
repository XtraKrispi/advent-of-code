module Main (main) where

import Control.Monad (guard)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

newtype Row = Row {fromRow :: Int}
  deriving (Eq, Ord)
  deriving newtype (Show)

newtype Col = Col {fromCol :: Int}
  deriving (Eq, Ord)
  deriving newtype (Show)
type Coord = (Row, Col)

type PaperGrid = Set Coord

adjacentPositions :: Coord -> [Coord]
adjacentPositions (Row row, Col col) =
  [ (Row (row - 1), Col (col - 1))
  , (Row (row - 1), Col (col))
  , (Row (row - 1), Col (col + 1))
  , (Row row, Col (col - 1))
  , (Row row, Col (col + 1))
  , (Row (row + 1), Col (col - 1))
  , (Row (row + 1), Col (col))
  , (Row (row + 1), Col (col + 1))
  ]

part1 :: PaperGrid -> Int
part1 input =
  Set.size $ Set.filter (isAccessible input) input

isAccessible :: PaperGrid -> Coord -> Bool
isAccessible grid coord =
  let adjacent = adjacentPositions coord
   in (< 4) $ length $ filter (\c -> Set.member c grid) adjacent

part2 :: PaperGrid -> Int
part2 input = 0

parseInput :: String -> PaperGrid
parseInput input =
  Set.fromList $
    do
      (rowIdx, row) <- zip [0 ..] (lines input)
      (colIdx, col) <- zip [0 ..] row
      guard $ col == '@'
      pure (Row rowIdx, Col colIdx)

main :: IO ()
main = do
  putStrLn "Sample: "
  sample <- parseInput <$> readFile "sample.txt"
  putStrLn $ "Part 1: " <> show (part1 sample)
  putStrLn $ "Part 2: " <> show (part2 sample)

  putStrLn "Input: "
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
