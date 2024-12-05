module Main where

import Control.Arrow ((&&&))
import Data.Bifunctor (Bifunctor (..))
import Data.IntMap (IntMap)
import Data.IntMap qualified as Map
import Data.List ((\\))

type Update = [Int]

data Input = Input (IntMap [Int]) [Update]
  deriving (Show)

type OrderChain = [Int]

part1 :: Input -> Int
part1 (Input rules updates) = sum $ middleNumber <$> filter (\u -> isValidUpdate rules [] u u) updates
 where
  middleNumber :: Update -> Int
  middleNumber xs = xs !! (length xs `div` 2)

isValidUpdate :: IntMap [Int] -> [Int] -> Update -> Update -> Bool
isValidUpdate _ _ _ [] = True
isValidUpdate rules prevs wholeLine (x : xs) =
  if isValid rules prevs wholeLine x
    then
      isValidUpdate rules (x : prevs) wholeLine xs
    else False

isValid :: IntMap [Int] -> [Int] -> [Int] -> Int -> Bool
isValid rules prevs wholeLine current =
  let precedences = Map.lookup current rules
   in case precedences of
        Nothing -> True
        Just ps -> (filter (\p -> p `elem` wholeLine) ps) \\ prevs == []

part2 :: Input -> Int
part2 _ = 0

makeInput :: String -> Input
makeInput contents =
  let (precedenceRules, update) =
        bimap makePrecedenceRules (makeUpdate . drop 1) $
          break (\s -> s == "") $
            lines contents
   in Input precedenceRules update
 where
  makeUpdate :: [String] -> [Update]
  makeUpdate ls = (\s -> read <$> splitAt' (\c -> c == ',') s) <$> ls
  makePrecedenceRules :: [String] -> IntMap [Int]
  makePrecedenceRules =
    foldr
      ( \s m ->
          let (x :: Int, y :: Int) = bimap read (read . drop 1) $ break (\c -> c == '|') s
           in Map.insertWith (++) y [x] m
      )
      Map.empty

splitAt' :: (Char -> Bool) -> String -> [String]
splitAt' p s = reverse $ go [] s
 where
  go :: [String] -> String -> [String]
  go accum "" = accum
  go accum (c : cs) =
    if p c
      then
        go ([] : accum) cs
      else case accum of
        [] -> go [[c]] cs
        (x : xs) -> go ((x ++ [c]) : xs) cs

main :: IO ()
main = makeInput <$> readFile "input.txt" >>= print . (part1 &&& part2)
