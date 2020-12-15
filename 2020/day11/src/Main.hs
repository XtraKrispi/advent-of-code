{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad (guard, (>=>))
import Control.Monad.Fix (fix)
import Control.Parallel.Strategies (NFData, parList, rdeepseq, using)
import Data.List (nubBy, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)

data Seat
  = Floor
  | OccupiedSeat
  | EmptySeat
  deriving (Show, Eq, Generic)

instance NFData Seat

type Row = Int

type Col = Int

type Position = (Row, Col)

type SeatingMap = Map Position Seat

type Rule = SeatingMap -> Position -> Seat

-- Only the closest seat to the point
columns :: Position -> SeatingMap -> [(Position, Seat)]
columns (row, col) seatingMap =
  let filtered = filter (\((r, c), _) -> r /= row && c == col) $ Map.assocs seatingMap
      bottom = take 1 $ sortBy (\((r, _), _) ((r', _), _) -> compare r r') $ filter (\((r, _), s) -> r > row && s /= Floor) filtered
      top = take 1 $ sortBy (\((r, _), _) ((r', _), _) -> compare r' r) $ filter (\((r, _), s) -> r < row && s /= Floor) filtered
   in bottom ++ top

rows :: Position -> SeatingMap -> [(Position, Seat)]
rows (row, col) seatingMap =
  let filtered = filter (\((r, c), _) -> c /= col && r == row) $ Map.assocs seatingMap
      right = take 1 $ sortBy (\((_, c), _) ((_, c'), _) -> compare c c') $ filter (\((r, c), s) -> c > col && s /= Floor) filtered
      left = take 1 $ sortBy (\((_, c), _) ((_, c'), _) -> compare c' c) $ filter (\((r, c), s) -> c < col && s /= Floor) filtered
   in left ++ right

diagonals :: Position -> SeatingMap -> [(Position, Seat)]
diagonals (row, col) seatingMap =
  catMaybes
    [ topLeft seatingMap (row - 1, col - 1),
      topRight seatingMap (maxCol - col) (row - 1, col + 1),
      bottomLeft seatingMap (maxRow - row) (row + 1, col - 1),
      bottomRight seatingMap (maxRow - row) (maxCol - col) (row + 1, col + 1)
    ]
  where
    maxRow = maximum $ fst <$> Map.keys seatingMap
    maxCol = maximum $ snd <$> Map.keys seatingMap

topLeft :: SeatingMap -> Position -> Maybe (Position, Seat)
topLeft seatingMap (row, col)
  | row < 0 || col < 0 = Nothing
  | row == 0 || col == 0 = ((row, col),) <$> mSeat
  | otherwise =
    if isJust mSeat
      then ((row, col),) <$> mSeat
      else topLeft seatingMap (row - 1, col - 1)
  where
    mSeat =
      Map.lookup (row, col) seatingMap
        >>= ( \s ->
                if s == OccupiedSeat || s == EmptySeat
                  then Just s
                  else Nothing
            )

topRight :: SeatingMap -> Col -> Position -> Maybe (Position, Seat)
topRight seatingMap maxCol (row, col)
  | row < 0 || col < 0 = Nothing
  | row == 0 || maxCol == 0 = ((row, col),) <$> mSeat
  | otherwise =
    if isJust mSeat
      then ((row, col),) <$> mSeat
      else topRight seatingMap (maxCol - 1) (row - 1, col + 1)
  where
    mSeat =
      Map.lookup (row, col) seatingMap
        >>= ( \s ->
                if s == OccupiedSeat || s == EmptySeat
                  then Just s
                  else Nothing
            )

bottomLeft :: SeatingMap -> Row -> Position -> Maybe (Position, Seat)
bottomLeft seatingMap maxRow (row, col)
  | row < 0 || col < 0 = Nothing
  | col == 0 || maxRow == 0 = ((row, col),) <$> mSeat
  | otherwise =
    if isJust mSeat
      then ((row, col),) <$> mSeat
      else bottomLeft seatingMap (maxRow - 1) (row + 1, col - 1)
  where
    mSeat =
      Map.lookup (row, col) seatingMap
        >>= ( \s ->
                if s == OccupiedSeat || s == EmptySeat
                  then Just s
                  else Nothing
            )

bottomRight :: SeatingMap -> Row -> Col -> Position -> Maybe (Position, Seat)
bottomRight seatingMap maxRow maxCol (row, col)
  | row < 0 || col < 0 = Nothing
  | maxCol == 0 || maxRow == 0 = ((row, col),) <$> mSeat
  | otherwise =
    if isJust mSeat
      then ((row, col),) <$> mSeat
      else bottomRight seatingMap (maxRow - 1) (maxCol - 1) (row + 1, col + 1)
  where
    mSeat =
      Map.lookup (row, col) seatingMap
        >>= ( \s ->
                if s == OccupiedSeat || s == EmptySeat
                  then Just s
                  else Nothing
            )

pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap f xs = map f xs `using` parList rdeepseq

surroundingPositions :: Position -> SeatingMap -> [(Position, Seat)]
surroundingPositions (r, c) seatingMap =
  (\(row, col) -> ((row, col), Map.findWithDefault Floor (row, col) seatingMap)) <$> do
    (r', c') <- (\(rFn, cFn) -> (rFn r, cFn c)) <$> coords
    guard $ r' >= 0 && c' >= 0 && r' <= maxRow && c' <= maxCol
    pure (r', c')
  where
    maxRow = maximum $ fst <$> Map.keys seatingMap
    maxCol = maximum $ snd <$> Map.keys seatingMap
    coords :: [(Int -> Int, Int -> Int)]
    coords =
      [ (subtract 1, subtract 1),
        (subtract 1, id),
        (subtract 1, (+ 1)),
        (id, subtract 1),
        (id, (+ 1)),
        ((+ 1), subtract 1),
        ((+ 1), id),
        ((+ 1), (+ 1))
      ]

part1Rules :: Rule
part1Rules seatingMap coord =
  let currentSeat = Map.findWithDefault Floor coord seatingMap
      surroundingSeats = snd <$> surroundingPositions coord seatingMap
   in case currentSeat of
        Floor -> Floor
        EmptySeat
          | OccupiedSeat `notElem` surroundingSeats -> OccupiedSeat
          | otherwise -> EmptySeat
        OccupiedSeat
          | length (filter (== OccupiedSeat) surroundingSeats) >= 4 -> EmptySeat
          | otherwise -> OccupiedSeat

part2Rules :: Rule
part2Rules seatingMap coord =
  let currentSeat = Map.findWithDefault Floor coord seatingMap
      surroundingSeats =
        snd
          <$> nubBy
            (\(pos, _) (pos', _) -> pos == pos')
            ( rows coord seatingMap
                ++ columns coord seatingMap
                ++ diagonals coord seatingMap
            )
   in case currentSeat of
        Floor -> Floor
        EmptySeat
          | OccupiedSeat `notElem` surroundingSeats -> OccupiedSeat
          | otherwise -> EmptySeat
        OccupiedSeat
          | length (filter (== OccupiedSeat) surroundingSeats) >= 5 -> EmptySeat
          | otherwise -> OccupiedSeat

runRules :: Rule -> SeatingMap -> SeatingMap
runRules rules seatingMap = Map.mapWithKey (\pos _ -> rules seatingMap pos) seatingMap

convert :: [String] -> SeatingMap
convert = Map.fromList . concatMap convertRow . zip [0 ..]
  where
    convertRow :: (Int, String) -> [(Position, Seat)]
    convertRow (row, cols) =
      ( \(col, c) ->
          ( (row, col),
            case c of
              'L' -> EmptySeat
              '#' -> OccupiedSeat
              _ -> Floor
          )
      )
        <$> zip [0 ..] cols

convertBack :: SeatingMap -> [String]
convertBack seatingMap =
  let maxCol = (1 +) $ maximum $ snd <$> Map.keys seatingMap
      go accum [] = accum
      go accum elems = go (take maxCol elems : accum) (drop maxCol elems)
      convertRow row =
        ( \case
            EmptySeat -> 'L'
            OccupiedSeat -> '#'
            Floor -> '.'
        )
          <$> row
   in (convertRow <$>) <$> reverse $ go [] $ Map.elems seatingMap

part1 :: SeatingMap -> Int
part1 = Map.size . Map.filter (== OccupiedSeat) . execute part1Rules

execute :: Rule -> SeatingMap -> SeatingMap
execute rule seatingMap =
  let result = runRules rule seatingMap
   in if result == seatingMap then seatingMap else execute rule result

writeData :: Int -> SeatingMap -> IO SeatingMap
writeData n s = do
  writeFile ("sample" <> show n <> ".txt") $ unlines $ convertBack s
  pure s

part2 :: SeatingMap -> Int
part2 = Map.size . Map.filter (== OccupiedSeat) . execute part2Rules

solve :: FilePath -> IO ()
solve = readFile >=> print . (part1) . convert . lines

main :: IO ()
main = solve "input.txt"
