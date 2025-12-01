module Main (main) where

import Control.Monad (when)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Foldable (foldlM, foldrM, traverse_)
import Data.Maybe (catMaybes)

data Direction = LeftDir | RightDir
  deriving (Eq)

data Rotation = Rotation {direction :: Direction, distance :: Int}

instance Show Rotation where
  show (Rotation LeftDir dist) = "L" <> show dist
  show (Rotation RightDir dist) = "R" <> show dist

data RotationResult = RotationResult {from :: Int, to :: Int, rotation :: Rotation}

instance Show RotationResult where
  show result = "From: " <> show result.from <> ", To: " <> show result.to <> ", Via: " <> show result.rotation

processRotations :: Int -> [Rotation] -> Writer [String] [RotationResult]
processRotations initial rotations =
  fmap snd $ do
    tell ["Initial: " <> show initial]
    tell ["Initial Rotations: " <> show rotations]
    foldlM
      ( \(current, rs) r -> do
          new <- processRotation current r
          pure (new, RotationResult{from = current, to = new, rotation = r} : rs)
      )
      (initial, [])
      rotations

processRotation :: Int -> Rotation -> Writer [String] Int
processRotation current rotation =
  let val =
        ( case rotation.direction of
            LeftDir -> current - rotation.distance
            RightDir -> current + rotation.distance
        )
          `mod` 100
   in do
        tell
          [ "Starting at: "
              <> show current
              <> ", rotating "
              <> show rotation
              <> ": "
              <> show val
          ]
        pure val

part1 :: [Rotation] -> (Int, [String])
part1 rs =
  let results = processRotations 50 rs
   in runWriter $ (\res -> length $ filter (\r -> r.to == 0) res) <$> results

part2 :: [Rotation] -> (Int, [String])
part2 rs =
  runWriter do
    results <- processRotations 50 rs
    sum <$> traverse passesZero (reverse results)
 where
  passesZero (RotationResult from _ r@(Rotation dir dist)) = do
    let n = numbers from dist dir
    tell ["Numbers for: " <> show from <> " and rotation: " <> show r <> ": " <> show n]
    pure $ length $ filter (\x -> x == 0) $ drop 1 n -- The starting point of 0 can't count, since it's already counted prior

numbers :: Int -> Int -> Direction -> [Int]
numbers from 0 _ = [from]
numbers from dist LeftDir
  | from == 0 = from : numbers 99 (dist - 1) LeftDir
  | otherwise = from : numbers (from - 1) (dist - 1) LeftDir
numbers from dist RightDir
  | from == 99 = from : numbers 0 (dist - 1) RightDir
  | otherwise = from : numbers (from + 1) (dist - 1) RightDir

parseInput :: String -> [Rotation]
parseInput input = catMaybes $ parseLine <$> lines input
 where
  parseLine ('L' : num) = Just $ Rotation LeftDir (read num)
  parseLine ('R' : num) = Just $ Rotation RightDir (read num)
  parseLine _ = Nothing

solve :: Bool -> String -> IO ()
solve shouldLog filePath = do
  file <- readFile filePath
  let rotations = parseInput file
  putStrLn $ "Part 1 " <> filePath <> ": "
  let (val, l) = part1 rotations
  putStrLn $ "Result: " <> (show val)
  when shouldLog do
    traverse_ putStrLn l
  putStrLn $ "Part 2 " <> filePath <> ": "
  let (val2, l2) = part2 rotations
  putStrLn $ "Result: " <> (show val2)
  when shouldLog do
    traverse_ putStrLn l2

main :: IO ()
main = do
  solve True "sample.txt"
  solve False "input.txt"