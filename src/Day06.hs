module Day06
  ( day06
  ) where

import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

redistribute :: Vector Int -> Vector Int
redistribute banks = V.accum (+) banks' additions
  where
    additions =
      [(i `mod` V.length banks, 1) | i <- take maxVal [(maxPos + 1) ..]]
    maxPos = V.maxIndex banks
    maxVal = banks ! maxPos
    banks' = banks // [(maxPos, 0)]

part1 :: [Int] -> Int
part1 blockCounts = go S.empty (iterate redistribute (V.fromList blockCounts))
  where
    go seen (x:xs)
      | x `S.notMember` seen = 1 + go (S.insert x seen) xs
    go _ _ = 0

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

parser :: Parser [Int]
parser = sepBy1 (read <$> many1 digit) (char '\t') <* newline

day06 :: IO ()
day06 =
  withInput "input/6.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ part1 parsed
