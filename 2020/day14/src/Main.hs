{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (Arrow ((&&&)))
import Control.Monad (sequence, (>=>))
import Data.Attoparsec.ByteString (Parser, many', string, takeByteString)
import Data.Attoparsec.ByteString.Char8 (digit, parseOnly)
import Data.Bits (Bits (setBit))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal (w2c)
import Data.Either (rights)
import qualified Data.IntMap as Map
import Debug.Trace (trace)
import Foreign (Bits (clearBit))

type MemoryLocation = Int

data BitOp = Clear | Set | Ignore
  deriving (Show, Eq)

data Instruction = UpdateBitMask [(Int, BitOp)] | WriteToMemory MemoryLocation Int
  deriving (Show)

type Program = [Instruction]

type Computer = Map.IntMap Int

instructionParser :: Parser Instruction
instructionParser = maskParser <|> memoryParser
  where
    maskParser = do
      string "mask = "
      convert <$> takeByteString
      where
        convert :: ByteString -> Instruction
        convert =
          UpdateBitMask
            . ( ( \(idx, char) ->
                    ( idx,
                      case w2c char of
                        '0' -> Clear
                        '1' -> Set
                        'X' -> Ignore
                    )
                )
                  <$>
              )
            . zip [0 ..]
            . reverse
            . B.unpack
    memoryParser = do
      string "mem["
      addr <- read <$> many' digit
      string "] = "
      val <- read <$> many' digit
      pure $ WriteToMemory addr val

part1 :: Program -> Int
part1 = sum . Map.elems . snd . foldl foldFn ([], Map.empty)
  where
    foldFn :: ([(Int, BitOp)], Map.IntMap Int) -> Instruction -> ([(Int, BitOp)], Map.IntMap Int)
    foldFn (_, map) (UpdateBitMask newMask) = (newMask, map)
    foldFn (mask, map) (WriteToMemory addr val) = (mask, Map.insert addr (applyMaskPart1 val mask) map)

applyMaskPart1 :: Int -> [(Int, BitOp)] -> Int
applyMaskPart1 =
  foldl
    ( \i (idx, op) -> case op of
        Set -> i `setBit` idx
        Clear -> i `clearBit` idx
        Ignore -> i
    )

part2 :: Program -> Int
part2 = sum . Map.elems . snd . foldl foldFn ([], Map.empty)
  where
    foldFn :: ([(Int, BitOp)], Map.IntMap Int) -> Instruction -> ([(Int, BitOp)], Map.IntMap Int)
    foldFn (_, map) (UpdateBitMask newMask) = (newMask, map)
    foldFn (mask, map) (WriteToMemory addr val) =
      let newAddresses = applyMaskPart2 addr mask
       in (mask, foldl (\m addr -> Map.insert addr val m) map newAddresses)

applyMaskPart2 :: Int -> [(Int, BitOp)] -> [Int]
applyMaskPart2 v =
  foldl
    ( \vals (idx, op) -> case op of
        Set -> (`setBit` idx) <$> vals
        Clear -> vals
        Ignore -> vals >>= (\val -> [setBit val idx, clearBit val idx])
    )
    [v]

solve :: FilePath -> IO ()
solve =
  C.readFile
    >=> print
      . (part1 &&& part2)
      . rights
      . (parseOnly instructionParser <$>)
      . C.lines

main :: IO ()
main = solve "input.txt"