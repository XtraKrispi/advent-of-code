module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.Map (Map)
import qualified Data.Map as Map

type Answer = Char

type Person = [Answer]

type Group = [Person]

parse :: [String] -> [Group]
parse = foldr foldFn []
  where
    foldFn s [] = [[s]]
    foldFn "" groups = [] : groups
    foldFn o (group : groups) = (o : group) : groups

distinctAnswers :: Group -> Map Char Int
distinctAnswers = foldr insertAnswers Map.empty
  where
    insertAnswers :: [Answer] -> Map Char Int -> Map Char Int
    insertAnswers a m = foldr (\c -> Map.insertWith (+) c 1) m a

part1 :: [Group] -> Int
part1 = sum . (length . Map.keys . distinctAnswers <$>)

part2 :: [Group] -> Int
part2 = sum . ((\g -> length . Map.keys . Map.filter (== length g) . distinctAnswers $ g) <$>)

solve :: String -> IO ()
solve fileName = do
  rows <- parse . lines <$> readFile fileName
  print $ part1 &&& part2 $ rows

main :: IO ()
main = solve "input.txt"
