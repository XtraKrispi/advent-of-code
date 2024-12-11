{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Arrow ((&&&))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt, intToDigit)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence (Seq ((:<|), (:|>)), (<|), (><), (|>))
import Data.Sequence qualified as Seq
import System.Environment (getArgs)

data Block = Empty | File Int
  deriving (Show, Eq)

newtype Id = Id Int
  deriving (Show)
newtype Length = Length Int
  deriving (Show)
newtype Index = Index Int
  deriving (Show, Eq, Ord, Num, Enum)

data Input = Input
  { blocks :: Map Index Block
  , empties :: Map Index Length
  , numbered :: Map Index (Length, Id)
  , numberOfBlocks :: Int
  }

instance Show Input where
  show (Input blocks _ _ _) =
    ( \b -> case b of
        Empty -> '.'
        File i -> intToDigit i
    )
      <$> Map.elems blocks
computeChecksum :: Input -> Int
computeChecksum (Input blocks _ _ _) =
  foldr
    ( \(Index idx, b) s -> case b of
        Empty -> s
        File i -> s + (i * idx)
    )
    0
    $ Map.assocs blocks

solve :: (Input -> Input) -> Input -> Int
solve fn = computeChecksum . go
 where
  go :: Input -> Input
  go blocks =
    let stepped = fn blocks
     in if isComplete stepped
          then
            stepped
          else go stepped

part1 :: Input -> Int
part1 = solve step

isComplete :: Input -> Bool
isComplete (Input blocks _ _ _) = uncurry (&&) $ bimap (all (/= Empty)) (all (== Empty)) $ break (== Empty) $ Map.elems blocks

step :: Input -> Input
step input =
  if isComplete input
    then
      input
    else case (first (\_ v -> v == Empty) input.blocks, Main.last (\_ v -> v /= Empty) input.blocks) of
      (Just kvp1, Just kvp2) -> input{blocks = (swap kvp1 kvp2 input.blocks)}
      _ -> error "Huh?"

swap :: (Ord k) => (k, v) -> (k, v) -> Map k v -> Map k v
swap (k1, v1) (k2, v2) = Map.insert k2 v1 . Map.insert k1 v2

first :: (Ord k) => (k -> v -> Bool) -> Map k v -> Maybe (k, v)
first p = listToMaybe . Map.assocs . Map.filterWithKey p

last :: (Ord k) => (k -> v -> Bool) -> Map k v -> Maybe (k, v)
last p = listToMaybe . reverse . Map.assocs . Map.filterWithKey p

step' :: Input -> Input
step' = id

apply :: (a -> a) -> a -> Int -> a
apply _ a 0 = a
apply fn a n = apply fn (fn a) (n - 1)

part2 :: Input -> Int
part2 _ = 0 -- solve step'

makeInput :: String -> Input
makeInput contents =
  let s = go (0, 0, Map.empty) contents
      (empties, blocks) = buildBlocks s
   in Input s empties blocks 0
 where
  go :: (Int, Index, Map Index Block) -> String -> Map Index Block
  go (currentId, idx, accum) (x : y : xs) =
    let blocks = zip [idx ..] (replicate (digitToInt x) (File currentId))
        blockIndex = maximum (fst <$> blocks) + 1
        empties = zip [blockIndex ..] (replicate (digitToInt y) Empty)
        emptiesIndex = if null empties then blockIndex else maximum (fst <$> empties) + 1
     in go (currentId + 1, emptiesIndex, insertInto empties (insertInto blocks accum)) xs
  go (currentId, idx, accum) [x] = insertInto (zip [idx ..] (replicate (digitToInt x) (File currentId))) accum
  go (_, _, accum) [] = accum

  buildBlocks :: Map Index Block -> (Map Index Length, Map Index (Length, Id))
  buildBlocks s = (Map.empty, Map.empty)

insertInto :: (Ord k) => [(k, v)] -> Map k v -> Map k v
insertInto vals m = foldr (uncurry Map.insert) m vals

main :: IO ()
main =
  getArgs
    <&> fromMaybe "sample.txt" . listToMaybe
    >>= readFile
    >>= print . (part1 &&& part2) . makeInput
