{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.Bifunctor (Bifunctor (first))
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Debug.Trace (trace)

-- [0,3,6]
-- [0, 3, 6, 0, 3, 3, 1, 0, 4]

{-
([6, 3,0], {0: [1], 3: [2], 6: [3] })

Looking at: 6
6 only spoken once, so 0
([0, 6, 3, 0], { 0: [4,1], 3: [2], 6: [3]})

Looking at: 0
0 spoken twice, so 4 - 1 = 3
([3, 0, 6, 3, 0], {0: [4,1], 3: [5, 2], 6: [3]})

Looking at: 3
3 spoken twice, so 5 - 2 = 3
([3,3,0,6,3,0], {0: [4,1], 3:[6,5,2], 6:[3]})
-}

newtype Turn = Turn {unTurn :: Int}
  deriving (Show, Eq, Num)

part1 :: Int -> [Int] -> Int
part1 n nums = snd $ head $ fst $ go (Turn n) $ seedData nums

seedData :: [Int] -> ([(Turn, Int)], IntMap [Turn])
seedData = foldl (\(nums, m) (idx, num) -> ((Turn idx, num) : nums, Map.insertWith (++) num [Turn idx] m)) ([], Map.empty) . zip [1 ..]

go :: Turn -> ([(Turn, Int)], IntMap [Turn]) -> ([(Turn, Int)], IntMap [Turn])
go idx ([], m) = ([], Map.empty)
go idx r@(last@(lastTurn, lastSpoken) : rest, m) =
  let thisNum =
        case Map.lookup lastSpoken m of
          Just (last : lastLast : _) ->
            unTurn $ last - lastLast
          _ ->
            0
      thisTurn = lastTurn + 1
   in if thisTurn == idx
        then ((thisTurn, thisNum) : last : rest, Map.insertWith (++) thisNum [thisTurn] m)
        else go idx ((thisTurn, thisNum) : last : rest, Map.insertWith (++) thisNum [thisTurn] m)

--(((nextIndex, nextNum) : (lastIndex, lastSpoken) : rest), )

part2 :: [Int] -> Int
part2 = const 0

solve :: Int -> [Int] -> IO ()
solve n = print . (part1 2020 &&& part2)

main :: IO ()
main = solve 2020 [9, 3, 1, 0, 8, 4]
