module Main (main) where

import Data.List (sort, sortOn)

type Joltage = Int

type Bank = [Joltage]

part1 :: [Bank] -> Int
part1 banks = sum $ largestInBank <$> banks

pairwiseVoltages :: Bank -> [Joltage]
pairwiseVoltages [] = []
pairwiseVoltages (x : rest) = something <> pairwiseVoltages rest
 where
  something = (\y -> toNum [x, y]) <$> rest

largestInBank :: Bank -> Joltage
largestInBank bank = maximum $ pairwiseVoltages bank

toNum :: [Int] -> Int
toNum = read . mconcat . (show <$>)

parseInput :: String -> [Bank]
parseInput s = (fmap (read . (: []))) <$> lines s

main :: IO ()
main = do
  putStrLn "Sample:"
  sample <- parseInput <$> readFile "sample.txt"
  putStrLn $ "Part 1: " <> show (part1 sample)

  putStrLn "Input:"
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
