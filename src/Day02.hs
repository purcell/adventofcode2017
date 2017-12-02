module Day02
  ( day02
  ) where

import Data.List (delete)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

difference :: [Int] -> Int
difference vals = maximum vals - minimum vals

evenDivision :: [Int] -> Int
evenDivision xs = head [q | x <- xs, y <- delete x xs, (q, 0) <- [x `divMod` y]]

checksumOn :: ([Int] -> Int) -> [[Int]] -> Int
checksumOn f = sum . fmap f

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

parser :: Parser [[Int]]
parser = many1 (sepBy1 number (char '\t') <* newline)
  where
    number :: Parser Int
    number = read <$> many1 digit

day02 :: IO ()
day02 =
  withInput "input/2.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ checksumOn difference parsed
    putStrLn "Part 2"
    print $ checksumOn evenDivision parsed
