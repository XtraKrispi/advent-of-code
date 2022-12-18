module Main where

getIndexOfDistinct :: Int -> [(Int, Char)] -> [(Int, Char)] -> Int
getIndexOfDistinct _ accum [] = fst $ last accum
getIndexOfDistinct num accum ((idx, x) : xs)
  | x `elem` fmap snd accum = getIndexOfDistinct num (trimTo x accum) ((idx, x) : xs)
  | length accum == num = idx
  | otherwise = getIndexOfDistinct num (accum ++ [(idx, x)]) xs

trimTo :: Char -> [(Int, Char)] -> [(Int, Char)]
trimTo _ [] = []
trimTo x (y : ys)
  | x == snd y = ys
  | otherwise = trimTo x ys

part1 :: String -> Int
part1 = (+ 1) . getIndexOfDistinct 3 [] . zip [0 ..]

part2 :: String -> Int
part2 = (+ 1) . getIndexOfDistinct 13 [] . zip [0 ..]

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
