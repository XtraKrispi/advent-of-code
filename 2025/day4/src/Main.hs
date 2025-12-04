module Main (main) where

import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map qualified as Map

newtype Row = Row Int
  deriving (Eq, Ord)
  deriving newtype (Show)

newtype Col = Col Int
  deriving (Eq, Ord)
  deriving newtype (Show)
type Coord = (Row, Col)

type PaperGrid = Map Coord Space

data Space = Empty | Roll
  deriving (Eq, Show)

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
  Map.size $ Map.filterWithKey (\c s -> if s == Empty then False else isAccessible input c) input

isAccessible :: PaperGrid -> Coord -> Bool
isAccessible grid coord =
  let adjacent = adjacentPositions coord
   in (< 4) $ length $ filter (\c -> maybe False (== Roll) $ Map.lookup c grid) adjacent

rollsOnly :: PaperGrid -> PaperGrid
rollsOnly = Map.filter (== Roll)

part2 :: PaperGrid -> (Int, [String])
part2 input = runWriter $ go 0 (Map.size $ rollsOnly input) input
 where
  go :: Int -> Int -> PaperGrid -> Writer [String] Int
  go removed prev grid = do
    tell ["In go with params: Removed: " <> show removed <> ", Prev: " <> show prev, prettyGrid grid]
    let accessible = Map.filterWithKey (\c s -> if s == Empty then False else isAccessible grid c) grid
        numAccessible = Map.size accessible
    tell ["accessible: " <> show accessible, "number of accessible: " <> show numAccessible]
    if Map.size accessible == 0
      then
        pure removed
      else do
        let updatedGrid = Map.foldrWithKey (\coord _ -> Map.insert coord Empty) grid accessible
        go (removed + numAccessible) numAccessible updatedGrid

parseInput :: String -> PaperGrid
parseInput input =
  Map.fromList $
    do
      (rowIdx, row) <- zip [0 ..] (lines input)
      (colIdx, col) <- zip [0 ..] row
      pure ((Row rowIdx, Col colIdx), if col == '@' then Roll else Empty)

prettyGrid :: PaperGrid -> String
prettyGrid grid =
  let minCoord = Map.lookupMin grid
      maxCoord = Map.lookupMax grid
   in case (minCoord, maxCoord) of
        (Just ((Row minRow, Col minCol), _), Just ((Row maxRow, Col maxCol), _)) ->
          unlines $
            ( \row ->
                ( \col ->
                    case Map.lookup (Row row, Col col) grid of
                      Nothing -> 'x'
                      Just Empty -> '.'
                      Just Roll -> '@'
                )
                  <$> [minCol .. maxCol]
            )
              <$> [minRow .. maxRow]
        _ -> ""

main :: IO ()
main = do
  putStrLn "Sample: "
  sample <- parseInput <$> readFile "sample.txt"
  putStrLn $ "Part 1: " <> show (part1 sample)
  let part2Sample = part2 sample
  putStrLn $ "Part 2: " <> show (fst part2Sample)
  putStrLn "Log: "
  traverse_ putStrLn (snd part2Sample)

  putStrLn "Input: "
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)

  putStrLn $ "Part 2: " <> show (fst $ part2 input)
