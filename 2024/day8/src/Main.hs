{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard, join)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type Coord = (Int, Int)

-- data Space = Space
--   { antenna :: Maybe Char
--   , antinodes :: [Char]
--   }

data Input = Input
  { minCoords :: Coord
  , maxCoords :: Coord
  , symbolMap :: Map Char [Coord]
  }
  deriving (Show)

part1 :: Input -> Int
part1 = Set.size . Set.fromList . join . Map.elems . antinodes

part2 :: Input -> Int
part2 _ = 0

pairs :: (Eq b) => [b] -> [(b, b)]
pairs xs = do
  x <- xs
  y <- xs
  guard $ x /= y
  pure (x, y)

dedupe :: (Eq a) => [(a, a)] -> [(a, a)]
dedupe =
  foldr
    ( \(a1, a2) results ->
        if (a2, a1) `elem` results
          then
            results
          else
            (a1, a2) : results
    )
    []

antinodes :: Input -> Map Char [Coord]
antinodes (Input minbounds maxbounds symbols) = computeAllAntinodes <$> symbols
 where
  computeAllAntinodes :: [Coord] -> [Coord]
  computeAllAntinodes coords = dedupe $ pairs coords >>= computeAntinodes (minbounds, maxbounds)
computeAntinodes :: (Coord, Coord) -> (Coord, Coord) -> [Coord]
computeAntinodes bounds (coord1@(r1, c1), coord2@(r2, c2)) =
  let colDiff = abs (c2 - c1)
      rowDiff = abs (r2 - r1)
   in filter
        (\coord -> isInBounds bounds coord && isOnLine coord1 coord2 coord && coord /= coord1 && coord /= coord2)
        -- This needs tweaking because we aren't accounting for the
        -- shape of the nodes
        [ (r1 - rowDiff, c1 - colDiff)
        , (r2 + rowDiff, c2 + colDiff)
        , (r1 + rowDiff, c1 - colDiff)
        , (r1 - rowDiff, c1 + colDiff)
        , (r2 + rowDiff, c2 - colDiff)
        , (r2 - rowDiff, c2 + colDiff)
        ]
isOnLine :: Coord -> Coord -> Coord -> Bool
isOnLine (row1, col1) (row2, col2) (row, col) = ((row - row1) `div` (row2 - row1)) == ((col - col1) `div` (col2 - col1))
isInBounds :: (Coord, Coord) -> Coord -> Bool
isInBounds ((minRow, minCol), (maxRow, maxCol)) (r, c) = r <= maxRow && r >= minRow && c <= maxCol && c >= minCol

makeInput :: String -> Input
makeInput contents =
  let initialMap = Map.fromListWith (++) do
        (row, line) <- zip [0 ..] (lines contents)
        (col, symbol) <- zip [0 ..] line
        pure (symbol, [(row, col)])
      allCoords = join $ Map.elems initialMap
   in Input
        (minimum allCoords)
        (maximum allCoords)
        (Map.filterWithKey (\k _ -> k /= '.') initialMap)

main :: IO ()
main =
  readFile "input.txt"
    >>= print
      . (part1 &&& part2)
      . makeInput
