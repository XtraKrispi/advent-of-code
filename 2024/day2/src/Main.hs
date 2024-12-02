module Main where

import Control.Arrow ((&&&))
import Data.List (sort)

type Level = Int
type Report = [Level]

type Input = [Report]

data FinalReports = FinalReports
  { safe :: [Report]
  , unsafe :: [Report]
  }
  deriving (Show)

part1 :: Input -> Int
part1 input = length $ safe $ foldr fileReport (FinalReports [] []) input

fileReport :: Report -> FinalReports -> FinalReports
fileReport report (FinalReports s u) =
  if isReportSafe report
    then FinalReports (report : s) u
    else FinalReports s (report : u)

isReportSafe :: Report -> Bool
isReportSafe levels =
  let ascending = sort levels
      descending = reverse ascending
   in (levels == ascending || levels == descending)
        && all
          (\l -> l >= 1 && l <= 3)
          ( fmap (\(a, b) -> abs (a - b)) $
              zip levels (drop 1 levels)
          )

part2 :: Input -> Int
part2 input =
  let finalReports = foldr fileReport (FinalReports [] []) input
   in length (safe finalReports)
        + ( foldr (\rs total -> if any isReportSafe rs then total + 1 else total) 0 $
              getPossibleReports <$> unsafe finalReports
          )

getPossibleReports :: Report -> [Report]
getPossibleReports report =
  foldr
    ( \(idx, _) reports ->
        ( (take idx report) ++ (drop (1 + idx) report)
        )
          : reports
    )
    []
    $ zip [0 ..] report

makeInput :: String -> Input
makeInput s = (fmap read . words) <$> lines s

main :: IO ()
main = do
  input <- makeInput <$> readFile "input.txt"
  print $ (part1 &&& part2) input
