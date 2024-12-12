{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import Data.Foldable (maximumBy)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)

-- Two options:
-- a) Map with indexes and for part 1 each block is of length 1
-- Then move entire blocks (works for part 1 and part 2)

-- b) Array with each block and scanning/swapping in place

data Block = Empty | File Int
  deriving (Show, Eq)

newtype Id = Id Int
  deriving (Show, Eq, Num, Ord)
newtype Length = Length Int
  deriving (Show, Num, Eq, Ord)
newtype Index = Index {unIndex :: Int}
  deriving (Show, Eq, Ord, Num, Enum)

newtype From a = From a
  deriving (Show)
newtype To a = To a
  deriving (Show)

data Input = Input
  { files :: Map Index (Length, Id)
  , numberOfBlocks :: Int
  }
  deriving (Show, Eq)

prettyInput :: Input -> String
prettyInput (Input files _) =
  snd $
    Map.foldlWithKey
      ( \(Index prevEnd, str) (Index idx) ((Length len), (Id id)) ->
          (Index (idx + len), str ++ intercalate " " (replicate (idx - prevEnd) ".") ++ " " ++ (intercalate " " (replicate len (show id))) ++ " ")
      )
      (0, "")
      files

computeChecksum :: Input -> Int
computeChecksum input =
  foldr
    ( \(Index idx, (Length l, Id i)) total ->
        (sum $ (* i) <$> [idx .. idx + l - 1]) + total
    )
    0
    $ Map.assocs input.files

solve :: Input -> Int
solve =
  computeChecksum
    . defrag Nothing
 where

defrag :: Maybe Id -> Input -> Input
defrag i input =
  let (i', input') = step i input
   in if input' == input
        then input
        else defrag i' input'

part1 :: Input -> Int
part1 = solve . expand
 where
  expand input = input{files = Map.foldrWithKey (\k v m -> m) Map.empty input.files}

step :: Maybe Id -> Input -> (Maybe Id, Input)
step m input =
  case findCandidate m input of
    Nothing -> (Nothing, input)
    Just (i, (from, to)) -> (Just i, move from to input)

findCandidate :: Maybe Id -> Input -> (Maybe (Id, (From Index, To Index)))
findCandidate m input = go m
 where
  go :: Maybe Id -> Maybe (Id, (From Index, To Index))
  go minSearchedId =
    case findFileBlock minSearchedId input of
      Nothing -> Nothing
      Just (maxIndex, (maxLength, i)) ->
        case findHole maxIndex maxLength input of
          Just idx -> Just $ (i, (From maxIndex, To idx))
          Nothing -> go (Just i)

findFileBlock :: Maybe Id -> Input -> Maybe (Index, (Length, Id))
findFileBlock minSearchedId input =
  Map.lookupMax $
    Map.filterWithKey
      (\_ (_, i) -> maybe True (\m -> i < m) minSearchedId)
      input.files

-- A hole is the min index where the difference between that index and the next is > length

findHole :: Index -> Length -> Input -> Maybe Index
findHole maxSearched (Length l) input = go (Map.assocs input.files) Nothing
 where
  go :: [(Index, (Length, Id))] -> Maybe Index -> Maybe Index
  go ((Index idx1, (Length len1, _)) : x@(Index idx2, _) : xs) result =
    if idx2 - (idx1 + len1) >= l && (Index idx2 < maxSearched)
      then Just (Index $ idx1 + len1)
      else go (x : xs) result
  go _ result = result

move :: From Index -> To Index -> Input -> Input
move (From fromIndex) (To toIndex) input =
  let
    newFiles :: Map Index (Length, Id)
    newFiles = case Map.lookup fromIndex input.files of
      Nothing -> input.files
      Just val -> Map.delete fromIndex $ Map.insert toIndex val input.files
   in
    input{files = newFiles}

apply :: (a -> a) -> a -> Int -> a
apply _ a 0 = a
apply fn a n = apply fn (fn a) (n - 1)

part2 :: Input -> Int
part2 = solve

makeInput :: String -> Input
makeInput contents =
  uncurry Input (buildBlocks (Id 0, Index 0, Map.empty) contents)
 where
  buildBlocks :: (Id, Index, Map Index (Length, Id)) -> String -> (Map Index (Length, Id), Int)
  buildBlocks (nextId, nextIndex, accum) (x : y : xs) =
    let
      len = Index $ digitToInt x
      empties = Index $ digitToInt y
     in
      buildBlocks (nextId + 1, nextIndex + len + empties, Map.insert nextIndex (Length (digitToInt x), nextId) accum) xs
  buildBlocks (nextId, nextIndex, accum) [x] =
    let result = Map.insert nextIndex (Length $ digitToInt x, nextId) accum
     in (result, unIndex $ nextIndex + Index (digitToInt x))
  buildBlocks (_, _, accum) _ = (accum, getTotal accum)
  getTotal :: Map Index (Length, Id) -> Int
  getTotal m =
    (\(Index idx, (Length l, _)) -> idx + l) $ maximumBy (\(a, _) (b, _) -> compare a b) $ Map.assocs m

insertInto :: (Ord k) => [(k, v)] -> Map k v -> Map k v
insertInto vals m = foldr (uncurry Map.insert) m vals

setup :: IO (Maybe Id, Input)
setup = do
  input <- makeInput <$> readFile "sample.txt"
  let (m, input') = step Nothing input
  let (m', input'') = step m input'
  let (m'', input''') = step m' input''
  pure (m'', input''')

main :: IO ()
main =
  getArgs
    <&> fromMaybe "sample.txt" . listToMaybe
    >>= readFile
    >>= print . (part1 &&& part2) . makeInput
