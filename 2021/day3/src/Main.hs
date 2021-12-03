module Main where

import Control.Arrow
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Debug.Trace
import Safe (headMay)
import System.Environment (getArgs)

type BinaryNumber = [Int]
type Input = [BinaryNumber]

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

maxesAndMins :: Input -> ([Int], [Int])
maxesAndMins =
  unzip
    . fmap
      ( bimap (snd . getMin . mconcat) (snd . getMax . mconcat)
          . unzip
          . map (\x -> (Min (length x, head x), Max (length x, head x)))
          . group
          . sort
      )

rate :: BinaryNumber -> Int
rate = toDec . (show =<<)

part1 :: Input -> Int
part1 =
  uncurry (*)
    . bimap rate rate
    . maxesAndMins
    . transpose

part2 :: Input -> Maybe Int
part2 input =
  (*)
    <$> filterNumbers Min getMin 0 input
    <*> filterNumbers Max getMax 0 input

filterNumbers :: Monoid m => ((Int, Int) -> m) -> (m -> (Int, Int)) -> Int -> Input -> Maybe Int
filterNumbers construct destruct _ [] = Nothing
filterNumbers construct destruct _ [x] = Just (rate x)
filterNumbers construct destruct idx xs =
  let digitToUse =
        (snd . destruct . mconcat)
          . map (\x -> construct (length x, head x))
          . group
          . sort
          $ transpose xs !! idx

      filtered = filter (\x -> x !! idx == digitToUse) xs
   in filterNumbers construct destruct (idx + 1) filtered

transform :: [String] -> Input
transform = (fmap (read . pure) <$>)

main :: IO ()
main = do
  fileName <- fromMaybe "input.txt" . headMay <$> getArgs
  readFile fileName >>= print . (part1 &&& part2) . transform . lines