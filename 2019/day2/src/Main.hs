module Main where

import Data.List.Split (splitOn)
import GHC.Arr (Array, elems, listArray, (!), (//))

interpret :: (Int, Array Int Int) -> Array Int Int
interpret (i, arr) =
  case arr ! i of
    1 ->
      let [i1, i2, i3] = (arr !) <$> [i + 1, i + 2, i + 3]
       in interpret (i + 4, arr // [(i3, arr ! i1 + arr ! i2)])
    2 ->
      let [i1, i2, i3] = (arr !) <$> [i + 1, i + 2, i + 3]
       in interpret (i + 4, arr // [(i3, arr ! i1 * arr ! i2)])
    99 -> arr
    _ -> error "Invalid input"

problem1 :: [Char] -> Int -> Int -> Int
problem1 input noun verb =
  let values = read <$> splitOn "," input
   in interpret
        ( 0,
          listArray (0, length values - 1) values
            // [(1, noun), (2, verb)]
        )
        ! 0

problem2 :: [Char] -> (Int, Int)
problem2 input =
  let nouns = [0 ..]
      verbs = [0 ..]
   in fst $
        head $
          dropWhile (\((n, v), o) -> o /= 19690720) $
            (\(n, v) -> ((n, v), problem1 input n v))
              <$> [ (noun, verb) | noun <- nouns, verb <- verbs
                  ]

main :: IO ()
main = do
  putStrLn "hello world"
