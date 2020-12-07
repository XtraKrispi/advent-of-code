module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((<=<))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (maximumBy)
import Data.List (sort)
import Data.Maybe (catMaybes, listToMaybe)

tupleToMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
tupleToMaybe (Just a, Just b) = Just (a, b)
tupleToMaybe _ = Nothing

data Partition = Lower | Upper
  deriving (Show)

search :: Int -> [Partition] -> [Int]
search n partitions = go partitions [0 .. n - 1]
  where
    go [] accum = accum
    go (part : parts) accum =
      let half = length accum `div` 2
       in case part of
            Lower -> go parts (take half accum)
            Upper -> go parts (drop half accum)

seatId :: (Int, Int) -> Int
seatId (row, col) = row * 8 + col

allSeatIds :: [String] -> Maybe [Int]
allSeatIds =
  (fmap seatId <$>)
    . sequence
    . ((tupleToMaybe . bimap (listToMaybe . search 128) (listToMaybe . search 8) . processBoardingPass) <$>)

part1 :: [String] -> Maybe Int
part1 =
  fmap maximum
    . allSeatIds

part2 :: [String] -> Maybe Int
part2 = checkSeat <=< fmap sort . allSeatIds
  where
    checkSeat :: [Int] -> Maybe Int
    checkSeat [] = Nothing
    checkSeat (a : b : rest)
      | b - a == 2 = pure (a + 1)
      | otherwise = checkSeat (b : rest)

processBoardingPass :: String -> ([Partition], [Partition])
processBoardingPass =
  foldr
    ( \c (rows, cols) ->
        case c of
          'F' -> (Lower : rows, cols)
          'B' -> (Upper : rows, cols)
          'L' -> (rows, Lower : cols)
          'R' -> (rows, Upper : cols)
    )
    ([], [])

main :: IO ()
main = do
  passes <- lines <$> readFile "input.txt"
  print $ part1 &&& part2 $ passes
