module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad (replicateM, (>=>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

type Coord = (Int, Int, Int)

type Coord2 = (Int, Int, Int, Int)

data CubeState = Active | Inactive
  deriving (Show, Eq)

type CubeMap = Map Coord CubeState

type CubeMap2 = Map Coord2 CubeState

part1 :: Int -> CubeMap -> Int
part1 numOfCycles cubes =
  Map.size . Map.filter (== Active) $ foldr (\_ m -> processCubesPart1 m) cubes [0 .. numOfCycles - 1]

processCubesPart1 :: CubeMap -> CubeMap
processCubesPart1 m = foldl foldFn Map.empty $ (,,) <$> [minX .. maxX] <*> [minY .. maxY] <*> [minZ .. maxZ]
  where
    foldFn m' coord = Map.insert coord (processCube m coord (Map.findWithDefault Inactive coord m)) m'
    allCoords = Map.keys m
    (minX, minY, minZ) = (xBound minimum allCoords - 1, yBound minimum allCoords - 1, zBound minimum allCoords - 1)
    (maxX, maxY, maxZ) = (xBound maximum allCoords + 1, yBound maximum allCoords + 1, zBound maximum allCoords + 1)
    xBound :: ([Int] -> Int) -> [Coord] -> Int
    xBound fn = fn . ((\(a, _, _) -> a) <$>)
    yBound :: ([Int] -> Int) -> [Coord] -> Int
    yBound fn = fn . ((\(_, a, _) -> a) <$>)
    zBound :: ([Int] -> Int) -> [Coord] -> Int
    zBound fn = fn . ((\(_, _, a) -> a) <$>)
    processCube :: CubeMap -> Coord -> CubeState -> CubeState
    processCube state coord s =
      let activeNeighbours = mapMaybe (\n -> (\s -> if s == Active then pure Active else Nothing) =<< Map.lookup n state) (neighbours coord) -- filter (== Active) $ (\n -> Map.findWithDefault Inactive n state) <$> neighbours coord
       in case length activeNeighbours of
            2 -> s
            3 -> Active
            _ -> Inactive
    neighbours :: Coord -> [Coord]
    neighbours (x, y, z) = tail [(x + dx, y + dy, z + dz) | [dx, dy, dz] <- replicateM 3 [0, 1, -1]]

part2 :: Int -> CubeMap -> Int
part2 numOfCycles cubes =
  let converted = Map.fromList $ (\((x, y, z), val) -> ((x, y, z, 0), val)) <$> Map.assocs cubes
   in Map.size . Map.filter (== Active) $ foldr (\_ m -> processCubesPart2 m) converted [0 .. numOfCycles - 1]

processCubesPart2 :: CubeMap2 -> CubeMap2
processCubesPart2 m = foldl foldFn Map.empty $ (,,,) <$> [minX .. maxX] <*> [minY .. maxY] <*> [minZ .. maxZ] <*> [minW .. maxW]
  where
    foldFn m' coord = Map.insert coord (processCube m coord (Map.findWithDefault Inactive coord m)) m'
    allCoords = Map.keys m
    (minX, minY, minZ, minW) = (xBound minimum allCoords - 1, yBound minimum allCoords - 1, zBound minimum allCoords - 1, wBound minimum allCoords - 1)
    (maxX, maxY, maxZ, maxW) = (xBound maximum allCoords + 1, yBound maximum allCoords + 1, zBound maximum allCoords + 1, wBound maximum allCoords + 1)
    xBound :: ([Int] -> Int) -> [Coord2] -> Int
    xBound fn = fn . ((\(a, _, _, _) -> a) <$>)
    yBound :: ([Int] -> Int) -> [Coord2] -> Int
    yBound fn = fn . ((\(_, a, _, _) -> a) <$>)
    zBound :: ([Int] -> Int) -> [Coord2] -> Int
    zBound fn = fn . ((\(_, _, a, _) -> a) <$>)
    wBound :: ([Int] -> Int) -> [Coord2] -> Int
    wBound fn = fn . ((\(_, _, _, a) -> a) <$>)
    processCube :: CubeMap2 -> Coord2 -> CubeState -> CubeState
    processCube state coord s =
      let activeNeighbours = mapMaybe (\n -> (\s -> if s == Active then pure Active else Nothing) =<< Map.lookup n state) (neighbours coord) -- filter (== Active) $ (\n -> Map.findWithDefault Inactive n state) <$> neighbours coord
       in case length activeNeighbours of
            2 -> s
            3 -> Active
            _ -> Inactive
    neighbours :: Coord2 -> [Coord2]
    neighbours (x, y, z, w) = tail [(x + dx, y + dy, z + dz, w + dw) | [dx, dy, dz, dw] <- replicateM 4 [0, 1, -1]]

convert :: [String] -> CubeMap
convert ls =
  let coordinated = zip [0 ..] $ zip [0 ..] <$> ls
   in Map.fromList [((x, y, 0), parseChar c) | (y, cs) <- coordinated, (x, c) <- cs]

parseChar :: Char -> CubeState
parseChar '#' = Active
parseChar _ = Inactive

solve :: FilePath -> IO ()
solve = readFile >=> print . (part1 6 &&& part2 6) . convert . lines

main :: IO ()
main = solve "input.txt"
