module Main (main) where

import Control.Monad (join)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.IntMap qualified as IntMap
import Data.List (nub, sortOn)

type Joltage = Int

type Bank = [Joltage]

part1 :: [Bank] -> Integer
part1 banks = sum $ determineLargestOfSize 2 <$> banks

part2 :: [Bank] -> Integer
part2 banks = sum $ determineLargestOfSize 12 <$> banks

determineLargestOfSize :: Int -> [Joltage] -> Integer
determineLargestOfSize n = maximum . fmap toNum . fst . runWriter . go n
 where
  go :: Int -> [Joltage] -> Writer [String] [Bank]
  go n' joltages | n' <= 0 || n' > length joltages = do
    pure [[]]
  go n' joltages
    | otherwise =
        let
          numbersAndIndices = reverse $ sortOn fst $ IntMap.toList $ foldr (\(idx, joltage) m -> IntMap.insertWith (++) joltage [idx] m) IntMap.empty $ zip [0 ..] joltages
          applicableIndices = take 1 $ filter (\(_, indices) -> not $ null indices) $ (\(num, indices) -> (num, filter (\idx -> (length joltages - idx) >= n') indices)) <$> numbersAndIndices
         in
          do
            -- newJoltages = case applicableIndices of
            --   [(number, indices)] -> (\idx -> (number :) <$> go (n' - 1) (drop idx joltages)) =<< indices
            --   _ -> []
            tell $
              [ "Joltages: " <> show joltages
              , "N: " <> show n'
              , "Numbers and Indices: " <> show numbersAndIndices
              , "Applicable Indices: " <> show applicableIndices
              ]

            case applicableIndices of
              [(number, indices)] ->
                join
                  <$> traverse
                    ( \idx -> do
                        results <- go (n' - 1) (drop (idx + 1) joltages)
                        pure $ nub $ (number :) <$> results
                    )
                    indices
              _ -> pure []

toNum :: [Int] -> Integer
toNum = read . mconcat . (show <$>)

parseInput :: String -> [Bank]
parseInput s = (fmap (read . (: []))) <$> lines s

main :: IO ()
main = do
  putStrLn "Sample:"
  sample <- parseInput <$> readFile "sample.txt"
  putStrLn $ "Part 1: " <> show (part1 sample)
  putStrLn $ "Part 2: " <> show (part2 sample)

  putStrLn "Input:"
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)
