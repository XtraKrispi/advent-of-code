module Main where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Data.List ((\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Debug.Trace (trace)

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

-- part1 :: Input -> Int
-- part1 :: Input -> Int
part1 (Input bounds m) =
  length $ filter (\s -> s == "XMAS" || s == reverse "XMAS") $ join $ (getWords . potentialWords bounds) <$> Map.keys m
 where
  getWords :: [[Coord]] -> [String]
  getWords ws = (\cs -> catMaybes $ flip Map.lookup m <$> cs) <$> ws

--   go 0 (Map.keys m)
--  where
--   go :: Int -> [Coord] -> Int
--   go count [] = count
--   go count (coord : xs) =
--     if isMatch coord then
--       go (count + 1)
--     -- if c == 'X' || c == 'S'
--     --   then case matches coord m of
--     --     [] -> go count xs
--     --     ms -> go (count + 1) (xs \\ join ms)
--     --   else go count xs
--   -- matches :: Coord -> Map Coord Char -> [Coord]
--   matches coord m =
--     let allWords = filter (\cs -> let word = snd <$> cs in word == "XMAS" || word == "SAMX") $ (\cs -> catMaybes $ (\c -> (c,) <$> Map.lookup c m) <$> cs) <$> potentialWords bounds coord
--      in allWords

part2 :: Input -> Int
part2 _ = 0

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

-- r r r r
{-
-- First row
row,col,   row,col + 1, row, col + 2, row, col + 3,

-- First column
row,col, row + 1, col, row+2, col, row+3, col

-- diag forward down
row,col, row+1,col+1, row+2,col+2, row+3,col+3

-- diag backward down
row,col, row+1,col-1, row+2,col-2, row+3,col-3

-}
