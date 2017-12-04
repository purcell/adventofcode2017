module Day04
  ( day04
  ) where

import Data.List (nub, sort)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

allUnique :: Eq a => [a] -> Bool
allUnique xs = xs == nub xs

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

parser :: Parser [[String]]
parser = many1 (sepBy1 (many1 lower) (char ' ') <* newline)

day04 :: IO ()
day04 =
  withInput "input/4.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ length $ filter allUnique parsed
    putStrLn "Part 2"
    print $ length $ filter allUnique $ fmap sort <$> parsed
