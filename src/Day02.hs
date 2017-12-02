module Day02
  ( day02
  ) where

import Data.Maybe
import Text.Parsec
import Text.Parsec.String

difference :: [Int] -> Int
difference vals = maximum vals - minimum vals

evenDivision :: [Int] -> Int
evenDivision = head . mapMaybe division . pairs
  where
    division (x, y)
      | x `mod` y == 0 = Just (x `div` y)
    division (x, y)
      | y `mod` x == 0 = Just (y `div` x)
    division _ = Nothing

pairs :: Eq a => [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = [(x, x') | x' <- xs] ++ pairs xs

checksumOn :: ([Int] -> Int) -> [[Int]] -> Int
checksumOn f = sum . fmap f

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile p path
  either (error . show) return result

parser :: Parser [[Int]]
parser = many1 (sepBy1 number (string "\t") <* newline)
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
