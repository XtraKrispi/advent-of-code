{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Arrow ((&&&))
import Data.List (nub, nubBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace (trace)

data Space = Obstruction | Empty
  deriving (Show, Eq)

type Row = Int
type Col = Int

type Coord = (Row, Col)

data Direction = North | South | East | West
  deriving (Show, Eq)

data Input = Input
  { guardPath :: [(Coord, Direction)]
  , wholeMap :: Map Coord Space
  , guardPosition :: (Coord, Direction)
  }
  deriving (Show)

part1 :: Input -> Int
part1 input = length $ nub $ fst <$> (guardPath $ fst $ simulate input)

simulate :: Input -> (Input, Bool)
simulate (Input path m g@(coord, dir)) =
  if g `elem` path
    then
      (Input path m g, True)
    else
      let next = nextCoord coord dir
       in case Map.lookup next m of
            Just Obstruction -> simulate (Input (g : path) m (coord, turnRight dir))
            Just Empty -> simulate (Input (g : path) m (next, dir))
            Nothing -> (Input (g : path) m g, False)

nextCoord :: Coord -> Direction -> Coord
nextCoord (row, col) North = (row - 1, col)
nextCoord (row, col) West = (row, col - 1)
nextCoord (row, col) South = (row + 1, col)
nextCoord (row, col) East = (row, col + 1)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

isInBounds :: Coord -> Map Coord Space -> Bool
isInBounds (row, col) m =
  let ((minRow, minCol), (maxRow, maxCol)) = (minimum &&& maximum) $ Map.keys m
   in row <= maxRow && row >= minRow && col <= maxCol && col >= minCol

part2 :: Input -> Int
part2 input@(Input _ m g) =
  let (Input p' _ _) = fst $ simulate input
      unusedCoords = fst <$> (nubBy (\(a, _) (b, _) -> a == b) $ filter (/= g) p')
      simulations =
        foldr
          ( \c sims ->
              -- Can we be smart about where we start?
              -- Look at the coordinate of the obstruction and start the guard right before that?
              let x = takeWhile (\gc -> fst gc /= c) p'
                  (_, isLooped) = simulate (Input [] (Map.insert c Obstruction m) g)
               in if isLooped
                    then 1 + sims
                    else sims
          )
          0
          unusedCoords
   in simulations

makeInput :: String -> Input
makeInput contents = uncurry (Input []) makeMap
 where
  makeMap :: (Map Coord Space, (Coord, Direction))
  makeMap =
    let allRows = zip [0 ..] $ lines contents
        allCols = (\(row, line) -> (\(col, c) -> ((row, col), c)) <$> zip [0 ..] line) =<< allRows
     in foldr
          ( \(coord, space) (m, g) ->
              case space of
                '#' -> (Map.insert coord Obstruction m, g)
                '^' -> (Map.insert coord Empty m, (coord, North))
                '>' -> (Map.insert coord Empty m, (coord, East))
                'v' -> (Map.insert coord Empty m, (coord, South))
                '<' -> (Map.insert coord Empty m, (coord, West))
                _ -> (Map.insert coord Empty m, g)
          )
          (Map.empty, ((-1, -1), North))
          allCols

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . makeInput
