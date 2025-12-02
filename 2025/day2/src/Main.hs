module Main (main) where

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
  invalidNumbers (IdRange s e) = filter (not . isValid . show) [s .. e]

isValid :: String -> Bool
isValid x
  | length x `mod` 2 /= 0 = True
  | otherwise =
      let leftHalf = take (length x `div` 2) x
          rightHalf = drop (length x `div` 2) x
       in leftHalf /= rightHalf

main :: IO ()
main = do
  putStrLn "Sample:"
  sample <- readFile "sample.txt"
  print $ part1 (parseInput sample)
  putStrLn "Input:"
  input <- readFile "input.txt"
  print $ part1 (parseInput input)
