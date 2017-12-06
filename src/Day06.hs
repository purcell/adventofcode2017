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

part1 :: Vector Int -> Int
part1 = fst . countUntilRepeated . redistributions

part2 :: Vector Int -> Int
part2 blockCounts = fst . countUntilRepeated $ fromFirstRepeat
  where
    fromFirstRepeat = snd . countUntilRepeated . redistributions $ blockCounts

redistributions :: Vector Int -> [Vector Int]
redistributions = iterate redistribute

countUntilRepeated :: [Vector Int] -> (Int, [Vector Int])
countUntilRepeated = go S.empty 0
  where
    go seen n (x:xs)
      | x `S.notMember` seen = go (S.insert x seen) (n + 1) xs
    go _ n xs = (n, xs)

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

parser :: Parser (Vector Int)
parser = V.fromList <$> sepBy1 (read <$> many1 digit) (char '\t') <* newline

day06 :: IO ()
day06 =
  withInput "input/6.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ part1 parsed
    putStrLn "Part 2"
    print $ part2 parsed
