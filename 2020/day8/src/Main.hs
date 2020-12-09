{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (Arrow ((&&&)))
import Data.Attoparsec.ByteString (Parser, string)
import Data.Attoparsec.ByteString.Char8 (anyChar, decimal, parseOnly)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

data OperationType = Acc | Jmp | Nop
  deriving (Show, Eq)

data Operation = Operation {operationType :: OperationType, operationValue :: Int}
  deriving (Show)

part1 :: [Operation] -> Int
part1 = go 0 (0, [])
  where
    go :: Int -> (Int, [Int]) -> [Operation] -> Int
    go idx (accum, prev) ops
      | idx `elem` prev = accum
      | otherwise =
        let op = ops !! idx
         in case operationType op of
              Acc -> go (idx + 1) (accum + operationValue op, idx : prev) ops
              Jmp -> go (idx + operationValue op) (accum, idx : prev) ops
              Nop -> go (idx + 1) (accum, idx : prev) ops

replaceOperations :: [Operation] -> [[Operation]]
replaceOperations ops = alter <$> zip [0 ..] (replicate (length opsToChange) ops)
  where
    opsToChange =
      map fst
        . filter
          (\(_, Operation operationType _) -> operationType == Jmp || operationType == Nop)
        $ zip [0 ..] ops
    alter (idx, singleRun) =
      let operationIndex = opsToChange !! idx
       in foldr (\(idx', operation) results -> (if idx' == operationIndex then swap operation else operation) : results) [] (zip [0 ..] singleRun)
    swap (Operation Jmp val) = Operation Nop val
    swap (Operation Nop val) = Operation Jmp val
    swap op = op

part2 :: [Operation] -> Int
part2 operations = fromMaybe 0 $ listToMaybe $ mapMaybe (go 0 (0, [])) $ replaceOperations operations
  where
    go :: Int -> (Int, [Int]) -> [Operation] -> Maybe Int
    go idx (accum, prev) ops
      | idx `elem` prev = Nothing
      | idx >= length ops = Just accum
      | otherwise =
        let op = ops !! idx
         in case operationType op of
              Acc -> go (idx + 1) (accum + operationValue op, idx : prev) ops
              Jmp -> go (idx + operationValue op) (accum, idx : prev) ops
              Nop -> go (idx + 1) (accum, idx : prev) ops

operationParser :: Parser Operation
operationParser =
  numericParser "acc" (Operation Acc)
    <|> numericParser "jmp" (Operation Jmp)
    <|> numericParser "nop" (Operation Nop)

numericParser :: ByteString -> (Int -> Operation) -> Parser Operation
numericParser prefix mk = mkData <$> (string (prefix <> " ") *> anyChar) <*> decimal
  where
    mkData '-' num = mk (- num)
    mkData _ num = mk num

solve :: FilePath -> IO ()
solve path = do
  operations <- mapMaybe (rightToMaybe . parseOnly operationParser) . C.lines <$> B.readFile path
  print $ part1 &&& part2 $ operations

main :: IO ()
main = do
  solve "input.txt"
