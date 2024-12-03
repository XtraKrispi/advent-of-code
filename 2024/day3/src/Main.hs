{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Text.Regex.Applicative (Alternative (many, (<|>)), RE, anySym, psym, sym, (=~))

type Input = String

data Token = OpOrMul OpOrMul | Unknown
  deriving (Show)

data OpOrMul = Op Op | Mul Mul
  deriving (Show)

data Op = Do | Dont
  deriving (Show)

type Mul = (Int, Int)

evalMul :: Mul -> Int
evalMul = uncurry (*)

getAllOps :: Input -> [OpOrMul]
getAllOps input = fromMaybe [] $ onlyOpsOrMuls <$> (input =~ many tokenRegex)
 where
  onlyOpsOrMuls =
    foldr
      ( \x results -> case x of
          OpOrMul opOrMul -> opOrMul : results
          _ -> results
      )
      []

tokenRegex :: RE Char Token
tokenRegex = OpOrMul <$> opOrMulRegex <|> Unknown <$ anySym

mulRegex :: RE Char Mul
mulRegex = do
  _ <- "mul("
  num1 <- many (psym isDigit)
  sym ','
  num2 <- many (psym isDigit)
  _ <- ")"
  pure (read num1, read num2)

opRegex :: RE Char Op
opRegex = (Dont <$ "don't()") <|> (Do <$ "do()")

opOrMulRegex :: RE Char OpOrMul
opOrMulRegex = Op <$> opRegex <|> Mul <$> mulRegex

part1 :: Input -> Int
part1 = sum . fmap evalMul . mulsOnly . getAllOps
 where
  mulsOnly =
    foldr
      ( \x result -> case x of
          Mul m -> m : result
          _ -> result
      )
      []

part2 :: Input -> Int
part2 = sum . fmap evalMul . evalOps Do [] . getAllOps
 where
  evalOps :: Op -> [Mul] -> [OpOrMul] -> [Mul]
  evalOps _ accum [] = accum
  evalOps _ accum (Op Do : xs) = evalOps Do accum xs
  evalOps _ accum (Op Dont : xs) = evalOps Dont accum xs
  evalOps Do accum (Mul m : xs) = evalOps Do (m : accum) xs
  evalOps Dont accum (Mul _ : xs) = evalOps Dont accum xs

makeInput :: String -> Input
makeInput = id

main :: IO ()
main = do
  input <- makeInput <$> readFile "input.txt"
  print $ (part1 &&& part2) input
