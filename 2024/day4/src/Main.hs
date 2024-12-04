module Main where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)

type Row = Int
type Col = Int
type Coord = (Row, Col)

data Input = Input Bounds (Map Coord Char)
  deriving (Show)

data Bounds = Bounds
  { minCoord :: Coord
  , maxCoord :: Coord
  }
  deriving (Show)

part1 :: Input -> Int
part1 (Input bounds m) =
  length $
    filter (\s -> s == "XMAS" || s == reverse "XMAS") $
      join $
        (getWords . potentialWords bounds) <$> Map.keys m
 where
  getWords :: [[Coord]] -> [String]
  getWords ws = (\cs -> catMaybes $ flip Map.lookup m <$> cs) <$> ws

part2 :: Input -> Int
part2 (Input bounds m) =
  length $
    filter (id) $
      catMaybes $
        (fmap (all isXmas . getWords) . potentialDiag bounds) <$> Map.keys m
 where
  isXmas :: String -> Bool
  isXmas s = s == "MAS" || s == "SAM"
  getWords :: [[Coord]] -> [String]
  getWords ws = (\cs -> catMaybes $ flip Map.lookup m <$> cs) <$> ws

makeInput :: String -> Input
makeInput contents =
  let rows = zip [0 ..] (lines contents)
      mapped = Map.fromList $ rows >>= (\(row, line) -> (\(col, char) -> ((row, col), char)) <$> zip [0 ..] line)
      minCoord = minimum $ Map.keys mapped
      maxCoord = maximum $ Map.keys mapped
   in Input (Bounds minCoord maxCoord) mapped

main :: IO ()
main = do
  input <- makeInput <$> readFile "input.txt"
  let Input bounds m = input
  print $ catMaybes $ potentialDiag bounds <$> Map.keys m

  print $ (part1 &&& part2) input

potentialWords :: Bounds -> Coord -> [[Coord]]
potentialWords
  (Bounds (minRow, minCol) (maxRow, maxCol))
  (row, col) =
    let coords =
          [ [(row, col), (row, col + 1), (row, col + 2), (row, col + 3)]
          , [(row, col), (row + 1, col), (row + 2, col), (row + 3, col)]
          , [(row, col), (row + 1, col + 1), (row + 2, col + 2), (row + 3, col + 3)]
          , [(row, col), (row + 1, col - 1), (row + 2, col - 2), (row + 3, col - 3)]
          ]
        isInBounds (r, c) = r >= minRow && r <= maxRow && c >= minCol && c <= maxCol
     in filter (all isInBounds) coords

potentialDiag :: Bounds -> Coord -> Maybe [[Coord]]
potentialDiag
  (Bounds (minRow, minCol) (maxRow, maxCol))
  (row, col) =
    let coords =
          [ [(row - 1, col - 1), (row, col), (row + 1, col + 1)]
          , [(row + 1, col - 1), (row, col), (row - 1, col + 1)]
          ]
        isInBounds (r, c) = r >= minRow && r <= maxRow && c >= minCol && c <= maxCol
     in if all (all isInBounds) coords then Just coords else Nothing
