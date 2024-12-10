module Main where

import Control.Arrow ((&&&))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Sequence (Seq ((:<|), (:|>)), (<|), (><), (|>))
import Data.Sequence qualified as Seq
import System.Environment (getArgs)

data Block = Empty | File Int
  deriving (Show, Eq)

type Input = Seq Block

computeChecksum :: Seq Block -> Int
computeChecksum blocks =
  Seq.foldrWithIndex
    ( \idx b s -> case b of
        Empty -> s
        File i -> s + (i * idx)
    )
    0
    blocks

part1 :: Input -> Int
part1 = computeChecksum . go
 where
  go :: Input -> Input
  go blocks =
    let stepped = step blocks
     in if isComplete stepped
          then
            stepped
          else go stepped

isComplete :: Input -> Bool
isComplete input = uncurry (&&) $ bimap (all (/= Empty)) (all (== Empty)) $ Seq.breakl (\b -> b == Empty) input

step :: Input -> Input
step input =
  if isComplete input
    then
      input
    else
      let (complete, incomplete) = Seq.breakl (\b -> b == Empty) input
          (block, rest) = move incomplete
       in (complete |> block) >< rest
 where
  move :: Seq Block -> (Block, Seq Block)
  move blocks =
    let (empties, nonEmptyRight) = Seq.breakr (\b -> b /= Empty) blocks
     in case nonEmptyRight of
          _ :<| (everything :|> block) -> (block, everything >< (Empty <| empties))
          _ -> (Empty, empties)

apply :: (a -> a) -> a -> Int -> a
apply _ a 0 = a
apply fn a n = apply fn (fn a) (n - 1)

part2 :: Input -> Int
part2 _ = 0

makeInput :: String -> Input
makeInput = go (0, Seq.empty)
 where
  go :: (Int, Seq Block) -> String -> Seq Block
  go (currentId, accum) (x : y : xs) = go (currentId + 1, accum >< Seq.replicate (digitToInt x) (File currentId) >< Seq.replicate (digitToInt y) Empty) xs
  go (currentId, accum) [x] = accum >< Seq.replicate (digitToInt x) (File currentId)
  go (_, accum) [] = accum

main :: IO ()
main =
  getArgs
    <&> fromMaybe "sample.txt" . listToMaybe
    >>= readFile
    >>= print . (part1 &&& part2) . makeInput
