{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (Arrow ((&&&)))
import Control.Monad (guard, join)
import Data.Attoparsec.ByteString (Parser, anyWord8, manyTill, parseOnly, sepBy, string)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Any (Any))

type Color = ByteString

type ContainmentRules = Map Color [(Int, Color)]

type ColorPath = [Color]

data ColorTree = Node {nodeColor :: Color, nodeNum :: Int, nodeChildren :: [ColorTree]}
  deriving (Show)

canHold :: Color -> [ColorTree] -> Bool
canHold colorToFind children =
  or $ mapFn <$> children
  where
    mapFn (Node color _ c)
      | color == colorToFind = True
      | otherwise = canHold colorToFind c

containmentParser :: Parser (Int, Color)
containmentParser = do
  num <- 0 <$ string "no" <|> decimal
  case num of
    0 -> pure (0, "")
    _ -> do
      color <- C.unwords . filter (/= " ") . C.words . B.pack <$> manyTill anyWord8 (string "bags " <|> string "bag " <|> string "bags" <|> string "bag")
      pure (num, color)

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` string ", "

ruleParser :: Parser (Color, [(Int, Color)])
ruleParser = do
  color <- B.pack <$> manyTill anyWord8 (string " bags contain ")
  rules <- filter ((0 /=) . fst) <$> commaSep containmentParser
  pure (color, rules)

parseRules :: [ByteString] -> ContainmentRules
parseRules = Map.fromList . rights . (parseOnly ruleParser <$>)

buildTree :: ContainmentRules -> Color -> ColorTree
buildTree rules color = go color (Node color 1 [])
  where
    go c (Node color' i subs) =
      case Map.lookup c rules of
        Nothing -> undefined
        Just paths ->
          let nodes = (\(num, col) -> go col (Node col num [])) <$> paths
           in Node color' i (subs ++ nodes)

findNumThatCanHold :: Color -> ContainmentRules -> Int
findNumThatCanHold colorToFind m =
  length . filter id $
    fmap (canHold colorToFind . nodeChildren) $
      buildTree m <$> Map.keys m

part1 :: Color -> [ByteString] -> Int
part1 color = findNumThatCanHold color . parseRules

findNumOfBagsHeld :: Color -> ContainmentRules -> Int
findNumOfBagsHeld colorToFind =
  go . nodeChildren . flip buildTree colorToFind
  where
    go children =
      sum $ (\(Node _ i children) -> i + go (join $ replicate i children)) <$> children

part2 :: Color -> [ByteString] -> Int
part2 color = findNumOfBagsHeld color . parseRules

solve :: Color -> FilePath -> IO ()
solve col path = do
  rules <- C.lines <$> B.readFile path
  print $ part1 col &&& part2 col $ rules

main :: IO ()
main = do
  solve "shiny gold" "input.txt"
