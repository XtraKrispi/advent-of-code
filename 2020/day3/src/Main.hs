module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.Bifunctor (Bifunctor (second), bimap, first)
import Data.Foldable (Foldable (fold))
import Data.Monoid (Product (Product), getProduct)

data Object = Empty | Tree
  deriving (Show, Eq)

type Pos = (Integer, Integer)

type Row = [(Integer, Object)]

convert :: [String] -> [Row]
convert = (fmap (second obj) . zip [0 ..] . cycle <$>)
  where
    obj '#' = Tree
    obj _ = Empty

allCoords :: (Integer -> Pos) -> [Pos]
allCoords fn = [fn x | x <- [0 ..]]

operate :: Int -> [Integer -> Pos] -> [Row] -> [(Int, Int)]
operate n fns m =
  let x = foldr foldFn (0, 0) . takeWhile (\(row, _) -> row <= toInteger (n - 1)) . allCoords
   in x <$> fns
  where
    foldFn :: Pos -> (Int, Int) -> (Int, Int)
    foldFn (row, col) results =
      case lookup col (m !! fromIntegral row) of
        Just Empty -> second (+ 1) results
        Just Tree -> first (+ 1) results
        _ -> results

part1 :: [String] -> (Int, Int)
part1 rows = head $ operate (length rows) [\x -> (x, x * 3)] . convert $ rows

part2 :: [String] -> (Int, Int)
part2 rows = bimap getProduct getProduct $ fold $ (bimap Product Product <$>) $ operate (length rows) [\x -> (x, x), \x -> (x, x * 3), \x -> (x, x * 5), \x -> (x, x * 7), \x -> (x * 2, x)] . convert $ rows

main :: IO ()
main = do
  rows <- lines <$> readFile "input.txt"
  print . (part1 &&& part2) $ rows
