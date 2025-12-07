module Main (main) where

import Control.Monad (join)
import Data.List (transpose)

data Operation = Add | Multiply
  deriving (Show)
data Problem = Problem {numbers :: [Integer], operation :: Operation}
  deriving (Show)

eval :: [Problem] -> Integer
eval = foldr evalProblem 0

evalProblem problem total =
  total
    + ( ( case problem.operation of
            Add -> sum
            Multiply -> product
        )
          problem.numbers
      )

parseInput :: String -> [Problem]
parseInput input =
  let allProblems = transpose . fmap words . lines
      foldFn x problem
        | x == "+" = problem{operation = Add}
        | x == "*" = problem{operation = Multiply}
        | otherwise = problem{numbers = read x : problem.numbers}
      createProblem xs = foldr foldFn (Problem [] Add) xs
   in createProblem <$> allProblems input

-- parseInputPart2 :: String -> [Problem]
parseInputPart2 :: String -> [Problem]
parseInputPart2 input =
  let allLines = lines input
      parsed :: [[Integer]] = fmap (fmap read) $ split $ transpose $ take (length allLines - 1) $ allLines
      operations :: [String] = join $ words <$> (drop (length allLines - 1) $ allLines)
   in (\(numbers, operation) -> Problem numbers (if operation == "*" then Multiply else Add)) <$> zip parsed (reverse operations)

split :: [String] -> [[String]]
split = go [[]]
 where
  go accum [] = accum
  go (acc : accum) (x : xs)
    | filter (/= ' ') x == [] = go ([] : acc : accum) xs
    | otherwise = go ((x : acc) : accum) xs

main :: IO ()
main = do
  putStrLn "Sample: "
  samplePart1 <- parseInput <$> readFile "sample.txt"
  samplePart2 <- parseInputPart2 <$> readFile "sample.txt"
  putStrLn $ "Part 1: " <> show (eval samplePart1)
  putStrLn $ "Part 2: " <> show (eval samplePart2)
  putStrLn "Input: "
  inputPart1 <- parseInput <$> readFile "input.txt"
  inputPart2 <- parseInputPart2 <$> readFile "input.txt"
  putStrLn $ "Part 1: " <> show (eval inputPart1)
  putStrLn $ "Part 2: " <> show (eval inputPart2)
