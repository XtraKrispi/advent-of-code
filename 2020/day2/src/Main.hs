{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow (Arrow ((&&&)))
import Data.Attoparsec.ByteString (Parser, anyWord8, parseOnly, string, takeByteString, word8)
import Data.Attoparsec.ByteString.Char8 (decimal)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word8)

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

data Line = Line
  { lineMin :: Integer,
    lineMax :: Integer,
    lineChar :: Word8,
    password :: ByteString
  }
  deriving (Show, Eq)

lineParser :: Parser Line
lineParser = do
  min <- decimal
  word8 (charToWord8 '-')
  max <- decimal
  word8 (charToWord8 ' ')
  char <- anyWord8
  string ": "
  Line min max char <$> takeByteString

isValidPart1 :: Line -> Bool
isValidPart1 (Line min max char password) = numOfChar >= min && numOfChar <= max
  where
    numOfChar =
      toInteger
        . BS.length
        . BS.filter (== char)
        $ password

part :: (Line -> Bool) -> [Line] -> (Int, Int)
part pred =
  foldr
    ( \line (val, inval) ->
        if pred line
          then (val + 1, inval)
          else (val, inval + 1)
    )
    (0, 0)

part1 :: [Line] -> (Int, Int)
part1 = part isValidPart1

isValidPart2 :: Line -> Bool
isValidPart2 (Line min max char password) =
  validate char $ foldr (foldFn min max) (Nothing, Nothing) (zip ([1 ..] :: [Integer]) (BS.unpack password))

validate :: Word8 -> (Maybe Word8, Maybe Word8) -> Bool
validate char (Just c, Nothing) = c == char
validate char (Nothing, Just c) = c == char
validate char (Just c1, Just c2) = c1 /= c2 && ((c1 == char) || (c2 == char))
validate _ _ = False

foldFn :: Integer -> Integer -> (Integer, Word8) -> (Maybe Word8, Maybe Word8) -> (Maybe Word8, Maybe Word8)
foldFn min max (pos, c) (pos1, pos2)
  | pos == min = (Just c, pos2)
  | pos == max = (pos1, Just c)
  | otherwise = (pos1, pos2)

part2 :: [Line] -> (Int, Int)
part2 = part isValidPart2

main :: IO ()
main = do
  passwords <- mapM (parseOnly lineParser) . BSC.lines <$> BS.readFile "input.txt"
  print $ (part1 &&& part2) <$> passwords
