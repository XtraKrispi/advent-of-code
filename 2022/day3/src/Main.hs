module Main where

import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

data Item = Item
  { itemChar :: Char,
    itemPriority :: Int
  }
  deriving (Show, Eq, Ord)

data Rucksack = Rucksack
  { rucksackCompartment1 :: [Item],
    rucksackCompartment2 :: [Item]
  }
  deriving (Show)

priorityMap :: Map Char Int
priorityMap =
  Map.fromList $
    zip ['a' .. 'z'] [1 .. 26]
      ++ zip ['A' .. 'Z'] [27 ..]

convertPriority :: Char -> Int
convertPriority = fromMaybe 0 . flip Map.lookup priorityMap

findRepeater :: [[Item]] -> Maybe Item
findRepeater [] = Nothing
findRepeater (x : xs) =
  case List.nub $ foldr List.intersect x xs of
    [] -> Nothing
    (x' : _) -> Just x'

buildRucksack :: [Char] -> Rucksack
buildRucksack xs =
  let halfway = length xs `div` 2
   in Rucksack
        ((\c -> Item c (convertPriority c)) <$> take halfway xs)
        ((\c -> Item c (convertPriority c)) <$> drop halfway xs)

part1 :: String -> Int
part1 =
  sum
    . fmap itemPriority
    . fromMaybe []
    . mapM (findRepeater . (\(Rucksack c1 c2) -> [c1, c2]) . buildRucksack)
    . lines

chunk :: [a] -> [[a]]
chunk [] = []
chunk (x : y : z : xs) = [x, y, z] : chunk xs
chunk _ = error "Not applicable"

-- part2 :: String -> Int
part2 :: String -> Int
part2 =
  sum
    . fromMaybe []
    . mapM (fmap itemPriority . findRepeater)
    . chunk
    . fmap (combine . buildRucksack)
    . lines
  where
    combine (Rucksack c1 c2) = c1 ++ c2

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
