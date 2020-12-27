{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Control.Monad (join, void, (>=>))
import Data.Attoparsec.ByteString (Parser, many', parseOnly, string, takeTill)
import Data.Attoparsec.ByteString.Char8 (char, decimal)
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import Data.List (group, isSubsequenceOf, nub, sort, sortBy, transpose)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)

type Fields = IntMap [ByteString]

type Ticket = [Int]

type TicketData = (Fields, Ticket, [Ticket])

type TicketSeats = [(Int, ByteString)]

allValues :: Fields -> [Int]
allValues = Map.keys

part1 :: TicketData -> Int
part1 (fields, _, nearby) = sum $ getInvalidFields fields =<< nearby

getInvalidFields :: Fields -> Ticket -> [Int]
getInvalidFields fields =
  let values = allValues fields
   in filter (`notElem` values)

part2 :: TicketData -> Int
part2 (fields, ticket, nearby) =
  let validTickets = getValidTickets fields nearby
      figuredOut = figureItOut (length validTickets) fields $ zip [0 ..] $ transpose validTickets
      sortFn (i, _) (i', _) = compare i i'
   in product . (fst <$>) . filter (("departure" `B.isInfixOf`) . snd) . zip ticket $ snd <$> sortBy sortFn figuredOut

getValidTickets :: Fields -> [Ticket] -> [Ticket]
getValidTickets fields = filter (null . getInvalidFields fields)

figureItOut :: Int -> IntMap [ByteString] -> [(Int, [Int])] -> [(Int, ByteString)]
figureItOut numberOfTickets fields vals = go validVals []
  where
    --head $
    validVals = (\(idx, vs) -> (idx, concatMap nub $ filter (\g -> length g == numberOfTickets) $ group $ sort $ concat $ (\v -> Map.findWithDefault [] v fields) <$> vs)) <$> (vals :: [(Int, [Int])])
    go :: [(Int, [ByteString])] -> [(Int, ByteString)] -> [(Int, ByteString)]
    go xs accum | all (null . snd) xs = accum
    go xs accum =
      let toRemove = second head <$> filter (\(_, ls) -> length ls == 1) xs
          allLabels = snd <$> toRemove
          filtered = second (filter (`notElem` allLabels)) <$> xs
       in go filtered (toRemove ++ accum)

solve :: FilePath -> IO ()
solve = C.readFile >=> print . ((part1 &&& part2) <$>) . parseOnly dataParser

rangeParser :: Parser [Int]
rangeParser =
  enumFromTo <$> decimal <* char '-' <*> decimal

fieldParser :: Parser [(Int, [ByteString])]
fieldParser = do
  fieldName <- takeTill ((== ':') . toEnum . fromEnum)
  string ": "
  firstRange <- rangeParser
  string " or "
  secondRange <- rangeParser
  pure $ fmap (,[fieldName]) (firstRange ++ secondRange)

ticketParser :: Parser Ticket
ticketParser = do
  nums <- many' (decimal <* char ',')
  lastNum <- decimal
  pure (nums ++ [lastNum])

newLine :: Parser ()
newLine = void $ string "\n"

dataParser :: Parser TicketData
dataParser = do
  fields <- many' (fieldParser <* newLine)
  newLine
  string "your ticket:"
  newLine
  ticket <- ticketParser <* newLine
  newLine
  string "nearby tickets:"
  newLine
  tickets <- many' (ticketParser <* newLine)
  pure (Map.fromListWith (++) $ join fields, ticket, tickets) --ticket, tickets)

main :: IO ()
main = solve "input.txt"
