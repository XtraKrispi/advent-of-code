{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard, join)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import System.Environment (getArgs)

type Coord = (Int, Int)

data Input = Input
  { minCoords :: Coord
  , maxCoords :: Coord
  , symbolMap :: Map Char [Coord]
  }
  deriving (Show)

type Computation = ((Coord, Coord) -> (Coord, Coord) -> [Coord])

part1 :: Input -> Int
part1 = compute computeAntinodes

part2 :: Input -> Int
part2 = compute computeAntinodes'

compute :: Computation -> Input -> Int
compute computation = Set.size . Set.fromList . join . Map.elems . antinodes computation

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

allGridCoords :: (Coord, Coord) -> [Coord]
allGridCoords ((minRow, minCol), (maxRow, maxCol)) = do
  row <- [minRow .. maxRow]
  col <- [minCol .. maxCol]
  pure $ (row, col)

antinodes :: Computation -> Input -> Map Char [Coord]
antinodes computation (Input minbounds maxbounds symbols) = computeAllAntinodes <$> symbols
 where
  computeAllAntinodes :: [Coord] -> [Coord]
  computeAllAntinodes coords = (dedupe $ pairs coords) >>= computation (minbounds, maxbounds)

computeAntinodes :: (Coord, Coord) -> (Coord, Coord) -> [Coord]
computeAntinodes bounds (coord1@(r1, c1), coord2@(r2, c2)) =
  let colDiff = abs (c2 - c1)
      rowDiff = abs (r2 - r1)
   in filter
        (\coord -> isInBounds bounds coord && isOnLine coord1 coord2 coord && coord /= coord1 && coord /= coord2)
        [ (r1 - rowDiff, c1 - colDiff)
        , (r2 + rowDiff, c2 + colDiff)
        , (r1 + rowDiff, c1 - colDiff)
        , (r1 - rowDiff, c1 + colDiff)
        , (r2 + rowDiff, c2 - colDiff)
        , (r2 - rowDiff, c2 + colDiff)
        ]

computeAntinodes' :: (Coord, Coord) -> (Coord, Coord) -> [Coord]
computeAntinodes' bounds (coord1, coord2) =
  filter
    (isOnLine coord1 coord2)
    (allGridCoords bounds)

isOnLine :: Coord -> Coord -> Coord -> Bool
isOnLine (row1, col1) (row2, col2) (row, col) =
  ((row - row1) `divide` (row2 - row1)) == ((col - col1) `divide` (col2 - col1))

divide :: Int -> Int -> Double
divide i j = fromIntegral i / fromIntegral j

isInBounds :: (Coord, Coord) -> Coord -> Bool
isInBounds ((minRow, minCol), (maxRow, maxCol)) (r, c) =
  r <= maxRow && r >= minRow && c <= maxCol && c >= minCol

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
main = do
  getArgs
    <&> fromMaybe "sample.txt" . listToMaybe
    >>= readFile
    >>= print
      . (part1 &&& part2)
      . makeInput
