module Main (main) where

import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (pack, splitOn, unpack)

data IdRange = IdRange {start :: Integer, end :: Integer}
  deriving (Show)
type Input = [IdRange]

parseInput :: String -> Input
parseInput input = catMaybes $ fmap parseRange $ splitOn "," $ pack input
 where
  parseRange range = case splitOn "-" range of
    [f, s] -> Just (IdRange (read (unpack f)) (read (unpack s)))
    _ -> Nothing

part1 :: Input -> Integer
part1 input = foldr (\range total -> total + sum (invalidNumbers range)) 0 input
 where
  invalidNumbers (IdRange s e) = filter (not . isValidPart1 . show) [s .. e]

isValidPart1 :: String -> Bool
isValidPart1 x
  | length x `mod` 2 /= 0 = True
  | otherwise =
      let leftHalf = take (length x `div` 2) x
          rightHalf = drop (length x `div` 2) x
       in leftHalf /= rightHalf

part2 :: Input -> Integer
part2 input = foldr (\range total -> total + sum (invalidNumbers range)) 0 input
 where
  invalidNumbers :: IdRange -> [Integer]
  invalidNumbers (IdRange s e) = filter (not . isValidPart2 . show) [s .. e]

isValidPart2 :: String -> Bool
isValidPart2 x =
  let factors = lengthFactors x
   in not $ any (allEqual . chunk x) factors
 where
  allEqual :: (Eq a) => [[a]] -> Bool
  allEqual xs = length (nub xs) == 1

  chunk :: [a] -> Int -> [[a]]
  chunk [] _ = []
  chunk xs i = take i xs : chunk (drop i xs) i

  lengthFactors :: String -> [Int]
  lengthFactors i = filter (\i' -> (length i `mod` i' == 0)) [1 .. length i - 1]

main :: IO ()
main = do
  sample <- parseInput <$> readFile "sample.txt"
  putStrLn "Sample Part 1:"
  print $ part1 sample

  putStrLn "Sample Part 2:"
  print $ part2 sample

  input <- parseInput <$> readFile "input.txt"
  putStrLn "Input Part 1:"
  print $ part1 input
  putStrLn "Input Part 2:"
  print $ part2 input
