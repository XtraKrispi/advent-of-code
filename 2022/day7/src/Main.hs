{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.Text (Parser, decimal, parseOnly, space, string, takeText)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

newtype FileSystem = FileSystem {unFileSystem :: [(Text, FileSystemEntry)]}
  deriving (Show)

data FileSystemEntryType = Directory | File Integer
  deriving (Show, Eq)

data FileSystemEntry = FileSystemEntry
  { entryType :: FileSystemEntryType
  , parent :: Maybe Text
  }
  deriving (Show)

part1 :: FileSystem -> Integer
part1 fs =
  foldr (\(k, _) total -> total + getSize fs k) 0 $
    filterFileSystem
      ( \f (k, entry) ->
          getSize f k <= 100000 && entry.entryType == Directory
      )
      fs

sampleFileSystem :: FileSystem
sampleFileSystem =
  FileSystem
    [ ("/", FileSystemEntry Directory Nothing)
    , ("a", FileSystemEntry Directory (Just "/"))
    , ("e", FileSystemEntry Directory (Just "a"))
    , ("i", FileSystemEntry (File 584) (Just "e"))
    , ("f", FileSystemEntry (File 29116) (Just "a"))
    , ("g", FileSystemEntry (File 2557) (Just "a"))
    , ("h.lst", FileSystemEntry (File 62596) (Just "a"))
    , ("b.txt", FileSystemEntry (File 14848514) (Just "/"))
    , ("c.dat", FileSystemEntry (File 8504156) (Just "/"))
    , ("d", FileSystemEntry Directory (Just "/"))
    , ("j", FileSystemEntry (File 4060174) (Just "d"))
    , ("d.log", FileSystemEntry (File 8033020) (Just "d"))
    , ("d.ext", FileSystemEntry (File 5626152) (Just "d"))
    , ("k", FileSystemEntry (File 7214296) (Just "d"))
    ]

getSize :: FileSystem -> Text -> Integer
getSize (FileSystem fs) file =
  case List.find (\(k, _) -> k == file) fs of
    Just (_, FileSystemEntry (File size) _) -> size
    Just (_, FileSystemEntry Directory _) ->
      let directChildren = filter (\(k, FileSystemEntry _ parent) -> parent == Just file) fs
       in foldr (\(k, _) totalSize -> totalSize + getSize (FileSystem fs) k) 0 directChildren
    Nothing -> 0

filterFileSystem :: (FileSystem -> (Text, FileSystemEntry) -> Bool) -> FileSystem -> [(Text, FileSystemEntry)]
filterFileSystem p f@(FileSystem fs) = filter (p f) fs

data Session
  = SessionCommand Command
  | SessionFileSystemEntry (Text, FileSystemEntry)

data Command = ChangeDirectory Text | ListDirectory
  deriving (Show)

parseCommand :: Parser Command
parseCommand =
  (string "$ cd " *> (ChangeDirectory <$> takeText))
    <|> (ListDirectory <$ string "$ ls")

parseFileSystemEntry :: Maybe Text -> Parser (Text, FileSystemEntry)
parseFileSystemEntry parent =
  parseDirectory <|> parseFile
 where
  parseDirectory =
    string "dir " *> ((,FileSystemEntry Directory parent) <$> takeText)
  parseFile = do
    size <- decimal
    void space
    fileName <- takeText
    pure (fileName, FileSystemEntry (File size) parent)

parseSession :: Maybe Text -> Parser Session
parseSession parent = SessionCommand <$> parseCommand <|> SessionFileSystemEntry <$> parseFileSystemEntry parent

generateFileSystem :: Maybe Text -> [(Text, FileSystemEntry)] -> [Text] -> [(Text, FileSystemEntry)]
generateFileSystem _ accum [] = accum
generateFileSystem currentParent accum (x : xs) =
  case parseOnly (parseSession currentParent) x of
    Left _ -> []
    Right (SessionCommand (ChangeDirectory "..")) ->
      generateFileSystem ((.parent) $ snd $ head accum) accum xs
    Right (SessionCommand (ChangeDirectory dir)) ->
      generateFileSystem (Just dir) accum xs
    Right (SessionCommand ListDirectory) -> generateFileSystem currentParent accum xs
    Right (SessionFileSystemEntry (key, entry)) -> generateFileSystem currentParent ((key, entry) : accum) xs

prettyPrintFileSystem :: FileSystem -> IO ()
prettyPrintFileSystem (FileSystem fs) = go 0 Nothing fs
 where
  go :: Int -> Maybe Text -> [(Text, FileSystemEntry)] -> IO ()
  go indentationLevel parent f = do
    let thisLevel = filter (\(k, entry) -> entry.parent == parent) f
    mapM_ (printFileSystemEntry indentationLevel) thisLevel
  leftPad n = T.pack (replicate n ' ')
  printFileSystemEntry :: Int -> (Text, FileSystemEntry) -> IO ()
  printFileSystemEntry indentationLevel (k, entry) =
    case entry.entryType of
      Directory -> do
        T.putStrLn $ leftPad indentationLevel <> "v " <> k
        go (indentationLevel + 1) (Just k) fs
      File size ->
        T.putStrLn $ leftPad indentationLevel <> k <> " " <> T.pack (show size)
main :: IO ()
main = do
  input <- T.readFile "input.txt"
  let result =
        FileSystem $
          generateFileSystem
            Nothing
            [("/", FileSystemEntry Directory Nothing)]
            (T.lines input)
  -- prettyPrintFileSystem result
  print $ part1 result
