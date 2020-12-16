module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad ((>=>))
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import Safe (readMay)

data Direction = East | West | North | South
  deriving (Show, Eq)

data Compass = Dir Direction | Forward
  deriving (Show, Eq)

data Rotation = LeftT | RightT
  deriving (Show, Eq)

data Action = Move Compass | Turn Rotation
  deriving (Show, Eq)

data NavInstr = NavInstr
  { navInstrAction :: Action,
    navInstrAmount :: Int
  }
  deriving (Show)

data Position = Position
  { positionLocation :: (Int, Int),
    positionFacing :: Direction
  }
  deriving (Show)

newtype Waypoint = Waypoint {unWaypoint :: (Int, Int)}
  deriving (Show)

rotate :: Rotation -> Direction -> Direction
rotate LeftT North = West
rotate LeftT South = East
rotate LeftT East = North
rotate LeftT West = South
rotate RightT North = East
rotate RightT South = West
rotate RightT East = South
rotate RightT West = North

part1 :: [NavInstr] -> Int
part1 = (\(Position (a, b) _) -> abs a + abs b) . foldl processInstructionPart1 (Position (0, 0) East)

processInstructionPart1 :: Position -> NavInstr -> Position
processInstructionPart1 (Position loc fac) (NavInstr (Turn rotation) rotationAmt) = Position loc $ foldr (\_ d -> rotate rotation d) fac [0 .. (rotationAmt `div` 90) - 1]
processInstructionPart1 pos@(Position (x, y) East) (NavInstr (Move Forward) moveAmt) = pos {positionLocation = (x + moveAmt, y)}
processInstructionPart1 pos@(Position (x, y) _) (NavInstr (Move (Dir East)) moveAmt) = pos {positionLocation = (x + moveAmt, y)}
processInstructionPart1 pos@(Position (x, y) West) (NavInstr (Move Forward) moveAmt) = pos {positionLocation = (x - moveAmt, y)}
processInstructionPart1 pos@(Position (x, y) _) (NavInstr (Move (Dir West)) moveAmt) = pos {positionLocation = (x - moveAmt, y)}
processInstructionPart1 pos@(Position (x, y) North) (NavInstr (Move Forward) moveAmt) = pos {positionLocation = (x, y + moveAmt)}
processInstructionPart1 pos@(Position (x, y) _) (NavInstr (Move (Dir North)) moveAmt) = pos {positionLocation = (x, y + moveAmt)}
processInstructionPart1 pos@(Position (x, y) South) (NavInstr (Move Forward) moveAmt) = pos {positionLocation = (x, y - moveAmt)}
processInstructionPart1 pos@(Position (x, y) _) (NavInstr (Move (Dir South)) moveAmt) = pos {positionLocation = (x, y - moveAmt)}

part2 :: [NavInstr] -> Int
part2 = (\(Position (a, b) _, _) -> abs a + abs b) . foldl processInstructionPart2 (Position (0, 0) East, Waypoint (10, 1))

processInstructionPart2 :: (Position, Waypoint) -> NavInstr -> (Position, Waypoint)
processInstructionPart2 (pos, waypoint) (NavInstr (Turn rotation) rotationAmt) = (pos, foldr (\_ d -> rotateWaypoint rotation d) waypoint [0 .. (rotationAmt `div` 90) - 1])
processInstructionPart2 (pos@(Position (x, y) _), waypoint@(Waypoint (wX, wY))) (NavInstr (Move Forward) amt) = (pos {positionLocation = (x + (wX * amt), y + (wY * amt))}, waypoint)
processInstructionPart2 (pos, Waypoint (x, y)) (NavInstr (Move (Dir North)) amt) = (pos, Waypoint (x, y + amt))
processInstructionPart2 (pos, Waypoint (x, y)) (NavInstr (Move (Dir South)) amt) = (pos, Waypoint (x, y - amt))
processInstructionPart2 (pos, Waypoint (x, y)) (NavInstr (Move (Dir East)) amt) = (pos, Waypoint (x + amt, y))
processInstructionPart2 (pos, Waypoint (x, y)) (NavInstr (Move (Dir West)) amt) = (pos, Waypoint (x - amt, y))

rotateWaypoint :: Rotation -> Waypoint -> Waypoint
rotateWaypoint LeftT (Waypoint (x, y)) = Waypoint (- y, x)
rotateWaypoint RightT (Waypoint (x, y)) = Waypoint (y, - x)

convert :: [String] -> [NavInstr]
convert = catMaybes . (convertStr <$>)
  where
    convertStr "" = Nothing
    convertStr [_] = Nothing
    convertStr (d : num) = NavInstr <$> convertAction d <*> readMay num
    convertAction 'N' = Just (Move (Dir North))
    convertAction 'S' = Just (Move (Dir South))
    convertAction 'E' = Just (Move (Dir East))
    convertAction 'W' = Just (Move (Dir West))
    convertAction 'F' = Just (Move Forward)
    convertAction 'L' = Just (Turn LeftT)
    convertAction 'R' = Just (Turn RightT)
    convertAction _ = Nothing

solve :: FilePath -> IO ()
solve = readFile >=> print . (part1 &&& part2) . convert . lines

main :: IO ()
main = solve "input.txt"
