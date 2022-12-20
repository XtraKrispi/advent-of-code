{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.Text (Parser, decimal, parseOnly, space, string, takeText)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

newtype FileSystem = FileSystem {unFileSystem :: Map ([Text], Text) FileSystemEntry}
  deriving (Show)

data FileSystemEntryType = Directory | File Integer
  deriving (Show, Eq)

newtype FileSystemEntry = FileSystemEntry {entryType :: FileSystemEntryType}
  deriving (Show, Eq)

part1 :: FileSystem -> Integer
part1 fs =
  foldr (\(k, _) total -> total + getSize fs k) 0 $
    filterFileSystem
      ( \f k entry ->
          getSize f k <= 100000 && entryType entry == Directory
      )
      fs

sampleFileSystem :: FileSystem
sampleFileSystem =
  FileSystem $
    Map.fromList
      [ (([], "/"), FileSystemEntry Directory)
      , ((["/"], "a"), FileSystemEntry Directory)
      , ((["/", "a"], "e"), FileSystemEntry Directory)
      , ((["/", "a", "e"], "i"), FileSystemEntry (File 584))
      , ((["/", "a"], "f"), FileSystemEntry (File 29116))
      , ((["/", "a"], "g"), FileSystemEntry (File 2557))
      , ((["/", "a"], "h.lst"), FileSystemEntry (File 62596))
      , ((["/"], "b.txt"), FileSystemEntry (File 14848514))
      , ((["/"], "c.dat"), FileSystemEntry (File 8504156))
      , ((["/"], "d"), FileSystemEntry Directory)
      , ((["/", "d"], "j"), FileSystemEntry (File 4060174))
      , ((["/", "d"], "d.log"), FileSystemEntry (File 8033020))
      , ((["/", "d"], "d.ext"), FileSystemEntry (File 5626152))
      , ((["/", "d"], "k"), FileSystemEntry (File 7214296))
      ]

getSize :: FileSystem -> ([Text], Text) -> Integer
getSize (FileSystem fs) file =
  case Map.lookup file fs of
    Just (FileSystemEntry (File size)) -> size
    Just (FileSystemEntry Directory) ->
      let directChildren = Map.filterWithKey (\(path, _) _ -> path == fst file ++ [snd file]) fs
       in Map.foldrWithKey (\k _ totalSize -> totalSize + getSize (FileSystem fs) k) 0 directChildren
    Nothing -> 0

filterFileSystem :: (FileSystem -> ([Text], Text) -> FileSystemEntry -> Bool) -> FileSystem -> [(([Text], Text), FileSystemEntry)]
filterFileSystem p f@(FileSystem fs) = Map.toList $ Map.filterWithKey (p f) fs

data Session
  = SessionCommand Command
  | SessionFileSystemEntry (([Text], Text), FileSystemEntry)

data Command = ChangeDirectory Text | ListDirectory
  deriving (Show)

parseCommand :: Parser Command
parseCommand =
  (string "$ cd " *> (ChangeDirectory <$> takeText))
    <|> (ListDirectory <$ string "$ ls")

parseFileSystemEntry :: [Text] -> Parser (([Text], Text), FileSystemEntry)
parseFileSystemEntry path =
  parseDirectory <|> parseFile
 where
  parseDirectory =
    string "dir " *> ((\dir -> ((path, dir), FileSystemEntry Directory)) <$> takeText)
  parseFile = do
    size <- decimal
    void space
    fileName <- takeText
    pure ((path, fileName), FileSystemEntry (File size))

parseSession :: [Text] -> Parser Session
parseSession path = SessionCommand <$> parseCommand <|> SessionFileSystemEntry <$> parseFileSystemEntry path

generateFileSystem :: [Text] -> [(([Text], Text), FileSystemEntry)] -> [Text] -> [(([Text], Text), FileSystemEntry)]
generateFileSystem _ accum [] = accum
generateFileSystem currentParent accum (x : xs) =
  case parseOnly (parseSession currentParent) x of
    Left _ -> []
    Right (SessionCommand (ChangeDirectory "..")) ->
      generateFileSystem (fst $ fst $ head accum) accum xs
    Right (SessionCommand (ChangeDirectory dir)) ->
      generateFileSystem (currentParent ++ [dir]) accum xs
    Right (SessionCommand ListDirectory) -> generateFileSystem currentParent accum xs
    Right (SessionFileSystemEntry (key, entry)) -> generateFileSystem currentParent ((key, entry) : accum) xs

prettyPrintFileSystem :: FileSystem -> IO ()
prettyPrintFileSystem (FileSystem fs) = go 0 [] (Map.toList fs)
 where
  go :: Int -> [Text] -> [(([Text], Text), FileSystemEntry)] -> IO ()
  go indentationLevel parent f = do
    let thisLevel = filter (\((p, k), _) -> p == parent) f
    mapM_ (printFileSystemEntry indentationLevel) thisLevel
  leftPad n = T.pack (replicate n ' ')
  printFileSystemEntry :: Int -> (([Text], Text), FileSystemEntry) -> IO ()
  printFileSystemEntry indentationLevel ((path, k), entry) =
    case entry.entryType of
      Directory -> do
        T.putStrLn $ leftPad indentationLevel <> "v " <> k
        go (indentationLevel + 1) (path ++ [k]) (Map.toList fs)
      File size ->
        T.putStrLn $ leftPad indentationLevel <> k <> " " <> T.pack (show size)

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  let result =
        FileSystem $
          Map.fromList $
            generateFileSystem
              []
              [(([], "/"), FileSystemEntry Directory)]
              (T.lines input)
  -- print $ foldl (\results (k, entry) -> (k, getSize result k) : results) [] (filterFileSystem (\fs _ e -> e == FileSystemEntry Directory) result)

  -- prettyPrintFileSystem result

  print $ part1 result
