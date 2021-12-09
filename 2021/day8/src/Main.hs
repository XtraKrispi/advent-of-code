module Main where

import Control.Arrow ((&&&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Safe (headMay)
import System.Environment (getArgs)

newtype SignalPattern = SignalPattern {unSignalPattern :: String}
  deriving (Show, Eq)
newtype OutputValue = OutputValue {unOutputValue :: String}
  deriving (Show)
newtype Signal = Signal {unSignal :: ([SignalPattern], [OutputValue])}
  deriving (Show)

newtype Input = Input {unInput :: [Signal]}
  deriving (Show)

type Digits = IntMap SignalPattern

digits :: Digits
digits =
  Map.fromAscList
    [ (0, SignalPattern "abcefg")
    , (1, SignalPattern "cf")
    , (2, SignalPattern "acdeg")
    , (3, SignalPattern "acdfg")
    , (4, SignalPattern "bcdf")
    , (5, SignalPattern "abdfg")
    , (6, SignalPattern "abdefg")
    , (7, SignalPattern "acf")
    , (8, SignalPattern "abcdefg")
    , (9, SignalPattern "abcdfg")
    ]

signalPatternLength :: SignalPattern -> Int
signalPatternLength = length . unSignalPattern

part1 :: Input -> Int
part1 = sum . (signalOutputs <$>) . unInput
 where
  applicableLengths = map (signalPatternLength . snd) $ filter (\(k, _) -> k `elem` [1, 4, 7, 8]) $ Map.toList digits
  signalOutputs = length . filter (\(OutputValue v) -> length v `elem` applicableLengths) . snd . unSignal

part2 :: Input -> Int
part2 = sum . (sum . decode <$>) . unInput

decode :: Signal -> [Int]
decode = undefined

transform :: String -> Input
transform = Input . (transformLine <$>) . lines
 where
  transformLine :: String -> Signal
  transformLine input = case splitOn "|" input of
    [signalPattern, outputValue] ->
      Signal
        ( SignalPattern <$> words signalPattern
        , OutputValue <$> words outputValue
        )
    _ -> Signal ([], [])

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  readFile fileName >>= print . (part1 &&& part2) . transform
