module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (group, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Safe (headMay)
import System.Environment (getArgs)

type Input = IntMap Int

part1 :: Input -> Int
part1 input = sum $ IntMap.elems $ foldr (const singleDay) input [0 .. 79]

part2 :: Input -> Int
part2 input = sum $ IntMap.elems $ foldr (const singleDay) input [0 .. 255]

singleDay :: Input -> Input
singleDay = IntMap.foldrWithKey foldFn IntMap.empty
 where
  foldFn 0 existingNum newMap =
    IntMap.insertWith (+) 8 existingNum (IntMap.insertWith (+) 6 existingNum newMap)
  foldFn key existing newMap = IntMap.insertWith (+) (key -1) existing newMap

transform :: String -> Input
transform = IntMap.fromAscList . ((\g -> (head g, length g)) <$>) . group . sort . (read <$>) . splitOn ","

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  readFile fileName >>= print . (part1 &&& part2) . transform
