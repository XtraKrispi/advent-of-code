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
import Data.Maybe (listToMaybe, mapMaybe)

data OperationType = Acc | Jmp | Nop
  deriving (Show, Eq)

data Operation = Operation {operationType :: OperationType, operationValue :: Int}
  deriving (Show)

data TerminationCondition = RepeatedOp | Eof
  deriving (Show, Eq)

runProgram :: (Int -> TerminationCondition -> a) -> (Operation -> Int -> Operation) -> Int -> (Int, [Int]) -> [Operation] -> a
runProgram tFn idxFn idx (accum, prev) ops
  | idx `elem` prev = tFn accum RepeatedOp
  | idx >= length ops = tFn accum Eof
  | otherwise =
    let op = idxFn (ops !! idx) idx
     in case operationType op of
          Acc -> runProgram tFn idxFn (idx + 1) (accum + operationValue op, idx : prev) ops
          Jmp -> runProgram tFn idxFn (idx + operationValue op) (accum, idx : prev) ops
          Nop -> runProgram tFn idxFn (idx + 1) (accum, idx : prev) ops

part1 :: [Operation] -> Int
part1 = runProgram const const 0 (0, [])

part2 :: [Operation] -> Maybe Int
part2 ops =
  listToMaybe
    . mapMaybe (\(idx, ops) -> runProgram terminationFunction (idxFn idx) 0 (0, []) ops)
    $ zip [0 ..] (replicate (length opsToChange) ops)
  where
    opsToChange =
      map fst
        . filter
          (\(_, Operation operationType _) -> operationType == Jmp || operationType == Nop)
        $ zip [0 ..] ops
    idxFn :: Int -> Operation -> Int -> Operation
    idxFn opIndex op idx = if opsToChange !! opIndex == idx then swap op else op
    terminationFunction _ RepeatedOp = Nothing
    terminationFunction accum Eof = pure accum
    swap (Operation Jmp val) = Operation Nop val
    swap (Operation Nop val) = Operation Jmp val
    swap op = op

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
