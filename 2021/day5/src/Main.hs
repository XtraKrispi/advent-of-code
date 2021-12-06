{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.Attoparsec.Text (
  Parser,
  char,
  decimal,
  parseOnly,
  string,
 )
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Safe (headMay)
import System.Environment (getArgs)

newtype XPos = XPos {unXPos :: Int}
  deriving (Show, Eq, Ord)
newtype YPos = YPos {unYPos :: Int}
  deriving (Show, Eq, Ord)

type Input = Map Position [LineSegment]
newtype Position = Position (XPos, YPos)
  deriving (Eq, Ord)

instance Show Position where
  show (Position (XPos x, YPos y)) = "(" <> show x <> "," <> show y <> ")"

data LineSegmentType = Horizontal | Vertical | Diagonal
  deriving (Show, Eq)
data LineSegment = LineSegment
  { starting :: Position
  , ending :: Position
  , segmentType :: LineSegmentType
  }
  deriving (Show, Eq)

part1 :: Input -> Int
part1 =
  length
    . filter (> 1)
    . fmap
      ( length
          . filter
            ( \ls ->
                segmentType ls == Horizontal
                  || segmentType ls == Vertical
            )
      )
    . Map.elems

part2 :: Input -> Int
part2 =
  length
    . filter (> 1)
    . fmap
      length
    . Map.elems

transform :: [Text] -> Input
transform lines =
  let parsed = rights . fmap (parseOnly lineParser) $ lines
   in foldl (\m (ls, p) -> Map.insertWith (++) p [ls] m) Map.empty $
        ( \xs -> do
            (ls, ps) <- xs
            p <- ps
            pure (ls, p)
        )
          $ (\ls -> (ls, expand ls)) <$> parsed

expand :: LineSegment -> [Position]
expand (LineSegment (Position (XPos x1, YPos y1)) (Position (XPos x2, YPos y2)) t) =
  case t of
    Horizontal ->
      let xs = if x2 > x1 then [x1 .. x2] else [x2 .. x1]
       in [Position (XPos x, YPos y1) | x <- xs]
    Vertical ->
      let ys = if y2 > y1 then [y1 .. y2] else [y2 .. y1]
       in [Position (XPos x1, YPos y) | y <- ys]
    Diagonal ->
      let dx = x2 - x1
          dy = y2 - y1
          xs = if x2 > x1 then [x1 .. x2] else [x2 .. x1]
       in [Position (XPos x, YPos y) | x <- xs, let y = y1 + dy * (x - x1) `div` dx]

lineParser :: Parser LineSegment
lineParser = do
  x1 <- decimal
  char ','
  y1 <- decimal
  string " -> "
  x2 <- decimal
  char ','
  y2 <- decimal
  pure
    ( LineSegment
        (Position (XPos x1, YPos y1))
        (Position (XPos x2, YPos y2))
        (getSegmentType (x1, y1) (x2, y2))
    )

getSegmentType :: (Int, Int) -> (Int, Int) -> LineSegmentType
getSegmentType (x1, y1) (x2, y2)
  | x1 == x2 = Vertical
  | y1 == y2 = Horizontal
  | otherwise = Diagonal

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  TIO.readFile fileName >>= print . (part1 &&& part2) . transform . T.lines
