{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow (Arrow ((&&&)))
import Data.Attoparsec.Text (Parser, decimal, parseOnly, string)
import Data.List (intersect)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Safe (readMay)
import System.Environment (getArgs)

type Key = Text

type Value = Text

type Passport = [(Key, Value)]

processField :: [Text] -> Maybe (Key, Value)
processField [key, value] = Just (key, value)
processField _ = Nothing

process :: Text -> [Passport] -> [Passport]
process str [] = [getFields str]
process "" ps = [] : ps
process str (p : ps) = (p ++ getFields str) : ps

getFields :: Text -> [(Key, Value)]
getFields = catMaybes . (processField . splitOn ":" <$>) . T.words

getPassports :: [Text] -> [Passport]
getPassports = foldr process []

required :: Map Text (Text -> Bool)
required =
  Map.fromList
    [ ("byr", validateBirthYear),
      ("iyr", validateIssueYear),
      ("eyr", validateExpirationYear),
      ("hgt", validateHeight),
      ("hcl", validateHairColor),
      ("ecl", validateEyeColor),
      ("pid", validatePassportId)
    ]

validateConvert :: Read a => (a -> Bool) -> Text -> Bool
validateConvert fn = fn . read . T.unpack

validateBirthYear :: Text -> Bool
validateBirthYear = validateConvert ((&&) <$> (<= 2002) <*> (>= 1920))

validateIssueYear :: Text -> Bool
validateIssueYear = validateConvert ((&&) <$> (<= 2020) <*> (>= 2010))

validateExpirationYear :: Text -> Bool
validateExpirationYear = validateConvert ((&&) <$> (<= 2030) <*> (>= 2020))

validateHeight :: Text -> Bool
validateHeight txt =
  case parseOnly heightParser txt of
    Left _ -> False
    Right (val, Inches) -> val >= 59 && val <= 76
    Right (val, Centimeters) -> val >= 150 && val <= 193

validateHairColor :: Text -> Bool
validateHairColor txt =
  case T.uncons txt of
    Just ('#', rest) -> T.length rest == 6 && T.all (\c -> c `elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']) rest
    _ -> False

validateEyeColor :: Text -> Bool
validateEyeColor = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

validatePassportId :: Text -> Bool
validatePassportId txt = T.length txt == 9 && isJust ((readMay . T.unpack $ txt) :: Maybe Int)

data HeightUnit = Inches | Centimeters

heightParser :: Parser (Int, HeightUnit)
heightParser = inchParser <|> centParser
  where
    inchParser = (,Inches) <$> decimal <* string "in"
    centParser = (,Centimeters) <$> decimal <* string "cm"

requiredFieldsPresent :: Passport -> Bool
requiredFieldsPresent ps = length ((fst <$> ps) `intersect` Map.keys required) == length required

part1 :: [Passport] -> Int
part1 = length . filter requiredFieldsPresent

part2 :: [Passport] -> Int
part2 = length . filter (\t -> requiredFieldsPresent t && validateRequirements t)

validateRequirements :: Passport -> Bool
validateRequirements ps = and $ (\(k, v) -> Just False /= (($ v) <$> Map.lookup k required)) <$> ps

main :: IO ()
main = do
  args <- getArgs
  passports <- getPassports . T.lines <$> TIO.readFile (fromMaybe "input.txt" $ listToMaybe args)
  print $ part1 &&& part2 $ passports
  pure ()
