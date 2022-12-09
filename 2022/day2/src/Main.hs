module Main (main) where

data RockPaperScissors
  = Rock
  | Paper
  | Scissors

data Result
  = Win
  | Lose
  | Draw

newtype Elf = Elf {unElf :: RockPaperScissors}

newtype Me = Me {unMe :: RockPaperScissors}

data Game = Game Elf Me

score :: Game -> Int
score (Game elf me) = myScore me + outcomeScore
  where
    myScore (Me Rock) = 1
    myScore (Me Paper) = 2
    myScore (Me Scissors) = 3

    outcomeScore =
      case determineWinner (unMe me) (unElf elf) of
        Win -> 6
        Draw -> 3
        Lose -> 0

determineWinner :: RockPaperScissors -> RockPaperScissors -> Result
determineWinner Paper Rock = Win
determineWinner Rock Scissors = Win
determineWinner Scissors Paper = Win
determineWinner Rock Paper = Lose
determineWinner Scissors Rock = Lose
determineWinner Paper Scissors = Lose
determineWinner _ _ = Draw

parseElf :: String -> Elf
parseElf "A" = Elf Rock
parseElf "B" = Elf Paper
parseElf "C" = Elf Scissors
parseElf _ = Elf Rock

parseSelf :: String -> Me
parseSelf "X" = Me Rock
parseSelf "Y" = Me Paper
parseSelf "Z" = Me Scissors
parseSelf _ = Me Rock

parseResult :: String -> Result
parseResult "X" = Lose
parseResult "Y" = Draw
parseResult "Z" = Win
parseResult _ = Draw

parseLine :: String -> Game
parseLine game =
  case words game of
    [elf, me] -> Game (parseElf elf) (parseSelf me)
    _ -> error "Invalid input"

part1 :: String -> Int
part1 = sum . fmap (score . parseLine) . lines

data UnfinishedGame = UnfinishedGame Elf Result

parseLinePart2 :: String -> UnfinishedGame
parseLinePart2 game = case words game of
  [elf, result] -> UnfinishedGame (parseElf elf) (parseResult result)
  _ -> error "Invalid input"

getPlayed :: RockPaperScissors -> Result -> RockPaperScissors
getPlayed Rock Win = Paper
getPlayed Rock Lose = Scissors
getPlayed Paper Win = Scissors
getPlayed Paper Lose = Rock
getPlayed Scissors Win = Rock
getPlayed Scissors Lose = Paper
getPlayed a Draw = a

finishGame :: UnfinishedGame -> Game
finishGame (UnfinishedGame (Elf e) r) = Game (Elf e) (Me (getPlayed e r))

part2 :: String -> Int
part2 = sum . fmap (score . finishGame . parseLinePart2) . lines

main :: IO ()
main = do
  d <- readFile "input.txt"
  print $ part1 d
  print $ part2 d
