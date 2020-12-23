{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.Bifunctor (Bifunctor (first))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
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
part1 n nums = snd $ fst $ go (Turn n) $ seedData nums

seedData :: [Int] -> ((Turn, Int), IntMap (Turn, Maybe Turn))
seedData = foldl (\(nums, m) (idx, num) -> ((Turn idx, num), Map.insert num (Turn idx, Nothing) m)) ((Turn 0, 1), Map.empty) . zip [1 ..]

go :: Turn -> ((Turn, Int), IntMap (Turn, Maybe Turn)) -> ((Turn, Int), IntMap (Turn, Maybe Turn))
go idx ((lastTurn, lastSpoken), m) =
  let thisNum =
        case Map.lookup lastSpoken m of
          Just (last, Just lastLast) ->
            unTurn $ last - lastLast
          _ ->
            0
      thisTurn = lastTurn + 1
      toInsert = case Map.lookup thisNum m of
        Just (last, Just lastLast) ->
          (thisTurn, Just last)
        Just (last, Nothing) ->
          (thisTurn, Just last)
        Nothing ->
          (thisTurn, Nothing)
   in if thisTurn == idx
        then ((thisTurn, thisNum), Map.insert thisNum toInsert m)
        else go idx ((thisTurn, thisNum), Map.insert thisNum toInsert m)

solve :: Int -> Int -> [Int] -> IO ()
solve n n' = print . (part1 n &&& part1 n')

main :: IO ()
main = solve 2020 30000000 [9, 3, 1, 0, 8, 4]
