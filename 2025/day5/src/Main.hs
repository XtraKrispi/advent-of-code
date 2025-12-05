module Main (main) where

import Data.Set (Set)
import Data.Set qualified as Set

type IngredientId = Integer

type IngredientRange = (IngredientId, IngredientId)

data Database = Database
  { freshIngredientRanges :: [IngredientRange]
  , ingredientIds :: Set IngredientId
  }
  deriving (Eq, Show)

part1 :: Database -> Int
part1 database = Set.size $ Set.filter (\ingredientId -> any (\(from, to) -> ingredientId >= from && ingredientId <= to) database.freshIngredientRanges) database.ingredientIds

getOverlapping :: IngredientRange -> [IngredientRange] -> [IngredientRange]
getOverlapping range ranges = filter (isOverlapping range) ranges
 where
  isOverlapping :: IngredientRange -> IngredientRange -> Bool
  isOverlapping (initialFrom, initialTo) (toCompareFrom, toCompareTo) =
    ( initialFrom
        <= toCompareFrom
        && initialTo <= toCompareTo
        && initialTo >= toCompareFrom
    )
      || (initialFrom >= toCompareFrom && initialTo >= toCompareTo && initialFrom <= toCompareTo)
      || (initialFrom >= toCompareFrom && initialTo <= toCompareTo)

determineNewRange :: [IngredientRange] -> IngredientRange
determineNewRange overlapping = (minimum $ fmap fst overlapping, maximum $ fmap snd overlapping)

reduce :: [IngredientRange] -> Integer
reduce [] = 0
reduce ranges@(x@(from, to) : rest) = do
  case getOverlapping x ranges of
    [] ->
      let toAdd = (to - from + 1)
       in toAdd + reduce rest
    [x']
      | x' == x ->
          let toAdd = (to - from + 1)
           in toAdd + reduce rest
    overlaps ->
      let (f', t') = determineNewRange overlaps
       in reduce ((f', t') : filter (\range -> not (range `elem` overlaps)) rest)

part2 :: Database -> Integer
part2 database = reduce database.freshIngredientRanges

parseInput :: String -> Database
parseInput input =
  let allLines = lines input
      ingredientRangeLines = takeWhile (\l -> l /= "") allLines
      ingredientLines = drop 1 $ dropWhile (\l -> l /= "") allLines
   in Database
        ( ( \rangeLine ->
              let firstIngredient = takeWhile (/= '-') rangeLine
                  secondIngredient = drop 1 $ dropWhile (/= '-') rangeLine
               in (read firstIngredient, read secondIngredient)
          )
            <$> ingredientRangeLines
        )
        (Set.fromList $ read <$> ingredientLines)

main :: IO ()
main = do
  putStrLn "Sample: "
  sample <- parseInput <$> readFile "sample.txt"
  putStrLn $ "Part 1: " <> show (part1 sample)
  putStrLn $ "Part 2: " <> show (part2 sample)

  putStrLn "Input: "
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
