module Day02
  ( day02
  ) where

import Text.Parsec
import Text.Parsec.String

difference :: [Int] -> Int
difference vals = (maximum vals) - (minimum vals)

checksum :: [[Int]] -> Int
checksum = sum . fmap difference

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
    print $ checksum parsed
