module Main (main) where

import Data.List (transpose)

data Operation = Add | Multiply
  deriving (Show)
data Problem = Problem {numbers :: [Integer], operation :: Operation}
  deriving (Show)

part1 :: [Problem] -> Integer
part1 = foldr evalProblem 0
 where
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

main :: IO ()
main = do
  putStrLn "hello world"
