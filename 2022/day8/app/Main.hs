module Main where

import Data.Bifunctor (Bifunctor (second))
import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

type Tree = Int

data Forest = Forest
  { trees :: Map (Int, Int) Tree
  , rows :: Int
  , cols :: Int
  }
  deriving (Show)

data ApplicableTrees = ApplicableTrees
  { north :: [Tree]
  , south :: [Tree]
  , east :: [Tree]
  , west :: [Tree]
  }
  deriving (Show)

applicableTrees :: (Int, Int) -> Forest -> ApplicableTrees
applicableTrees (r, c) (Forest trees rows cols) =
  ApplicableTrees
    { north = (\row -> fromMaybe 10 $ Map.lookup (row, c) trees) <$> [0 .. r - 1]
    , south = (\row -> fromMaybe 10 $ Map.lookup (row, c) trees) <$> [r + 1 .. rows - 1]
    , east = (\col -> fromMaybe 10 $ Map.lookup (r, col) trees) <$> [c + 1 .. cols - 1]
    , west = (\col -> fromMaybe 10 $ Map.lookup (r, col) trees) <$> [0 .. c - 1]
    }

isVisibleInLine :: Tree -> [Tree] -> Bool
isVisibleInLine tree = all (\t -> t < tree)

isVisibleFromOutside :: Tree -> ApplicableTrees -> Bool
isVisibleFromOutside tree (ApplicableTrees{north, south, east, west}) =
  any id $ isVisibleInLine tree <$> [north, south, east, west]

day1 :: Forest -> Int
day1 forest@(Forest trees _ _) =
  length $
    filter id $
      (\(coord, tree) -> isVisibleFromOutside tree $ applicableTrees coord forest) <$> Map.assocs trees

parse :: String -> Forest
parse input =
  let rows = (\(row, cs) -> (row, zip [0 ..] $ digitToInt <$> cs)) <$> (zip [0 ..] $ lines input)
      cols = length $ second head <$> rows
      treeMap = Map.fromList $ concat $ (\(row, cs) -> (\(col, t) -> ((row, col), t)) <$> cs) <$> rows
   in Forest{trees = treeMap, rows = (length rows), cols = cols}

main :: IO ()
main = putStrLn "Hello, Haskell!"
