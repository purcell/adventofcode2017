module Day04
  ( day04
  ) where

import Data.List (nubBy)
import Data.Map (Map)
import qualified Data.Map as M
import Prelude hiding (Word, words)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

type Phrase = [Word]

type Word = String

validBy :: (Word -> Word -> Bool) -> Phrase -> Bool
validBy comparator words = length words == length (nubBy comparator words)

isAnagramOf :: Word -> Word -> Bool
isAnagramOf a b = charHistogram a == charHistogram b

charHistogram :: Word -> Map Char Int
charHistogram = foldr (M.unionWith (+)) M.empty . fmap (`M.singleton` 1)

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
    print $ length $ filter (validBy (==)) parsed
    putStrLn "Part 2"
    print $ length $ filter (validBy isAnagramOf) parsed
