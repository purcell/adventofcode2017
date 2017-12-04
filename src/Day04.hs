module Day04
  ( day04
  ) where

import Data.List (nub)
import Prelude hiding (Word, words)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

type Phrase = [Word]

type Word = String

valid :: Phrase -> Bool
valid words = length words == length (nub words)

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

parser :: Parser [Phrase]
parser = many1 (sepBy1 (many1 lower) (char ' ') <* newline)

day04 :: IO ()
day04 =
  withInput "input/4.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ length $ filter valid parsed
