module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (group, sort, (\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Safe (headMay)
import System.Environment (getArgs)

type Board = IntMap ((Int, Int), Bool)

data Input = Input [Int] [Board]
  deriving (Show)

checkWinner :: Board -> Bool
checkWinner board = checkRows || checkCols
 where
  check op =
    any ((== 5) . length)
      . group
      . sort
      . map (op . fst)
      . filter snd
      $ IntMap.elems board
  checkRows = check snd
  checkCols = check fst

getUnmarked :: Board -> [Int]
getUnmarked = IntMap.keys . IntMap.filter (not . snd)

getMarked :: Board -> [Int]
getMarked = IntMap.keys . IntMap.filter snd

markValue :: Int -> Board -> Board
markValue =
  IntMap.update (\(pos, _) -> Just (pos, True))

score :: (Int, [Int]) -> Int
score (x, nums) = sum nums * x

--part1 :: Input -> Int
part1 :: Input -> Int
part1 (Input nums boards) =
  maybe 0 score $
    go nums boards
 where
  go [] _ = Nothing
  go (x : xs) bs =
    let updated = fmap (markValue x) bs
        filtered = filter checkWinner updated
     in if null filtered
          then go xs updated
          else Just (x, getUnmarked $ head filtered)

part2 :: Input -> Int
part2 (Input nums boards) =
  score . last $
    go nums boards []
 where
  go :: [Int] -> [Board] -> [(Int, [Int])] -> [(Int, [Int])]
  go [] _ accum = accum
  go (x : xs) bs accum =
    let updated = fmap (markValue x) bs
        filtered = filter checkWinner updated
        newBoards = updated \\ filtered
     in go xs newBoards (accum ++ ((\b -> (x, getUnmarked b)) <$> filtered))

parseDrawnNums :: String -> [Int]
parseDrawnNums = (read <$>) . splitOn ","

parseBoard :: [String] -> Board
parseBoard xs = IntMap.fromList $ do
  (row, rowS) <- zip [0 ..] xs
  (col, colS) <- zip [0 ..] (words rowS)
  pure (read colS, ((col, row), False))

transform :: [String] -> Input
transform lines =
  let drawnNums = parseDrawnNums (head lines)
      startingPoint = drop 2 lines
      getBoardRows = take 5
      go [] accum = accum
      go xs accum = go (drop 6 xs) (accum ++ [parseBoard (getBoardRows xs)])
   in Input drawnNums (go startingPoint [])

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  readFile fileName >>= print . (part1 &&& part2) . transform . lines