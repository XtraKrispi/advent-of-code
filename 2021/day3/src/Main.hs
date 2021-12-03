module Main where

import Control.Arrow
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Debug.Trace
import Safe (headMay)
import System.Environment (getArgs)

type BinaryNumber = [Int]
type Input = [BinaryNumber]

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

groupUp :: [BinaryNumber] -> ([Min (Int, Int)], [Max (Int, Int)])
groupUp = unzip . map (\x -> (Min (length x, head x), Max (length x, head x)))

groupings :: Input -> [([Min (Int, Int)], [Max (Int, Int)])]
groupings = fmap (groupUp . group . sort)

maxesAndMins :: [([Min (Int, Int)], [Max (Int, Int)])] -> [(Int, Int)]
maxesAndMins = fmap ((\(Min (_, a), Max (_, b)) -> (a, b)) . bimap mconcat mconcat)

rate :: BinaryNumber -> Int
rate = toDec . (show =<<)

--part1 :: Input -> Int
part1 :: Input -> Int
part1 =
  uncurry (*)
    . bimap rate rate
    . unzip
    . maxesAndMins
    . groupings
    . transpose

part2 :: Input -> Int
part2 input = 0

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  readFile fileName >>= print . (part1 &&& part2) . (fmap (read . pure) <$>) . lines