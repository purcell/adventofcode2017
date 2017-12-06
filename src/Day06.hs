module Day06
  ( day06
  ) where

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

redistribute :: Vector Int -> Vector Int
redistribute banks = V.accum (+) banks changes
  where
    changes =
      (maxPos, -maxVal) :
      [(i `mod` V.length banks, 1) | i <- take maxVal [(maxPos + 1) ..]]
    maxPos = V.maxIndex banks
    maxVal = banks ! maxPos

part1 :: Vector Int -> Int
part1 = fst . detectLoop

part2 :: Vector Int -> Int
part2 = snd . detectLoop

detectLoop :: Vector Int -> (Int, Int)
detectLoop = go M.empty 0 . iterate redistribute
  where
    go seen n (x:xs) =
      case x `M.lookup` seen of
        Just prev -> (n, n - prev)
        Nothing -> go (M.insert x n seen) (n + 1) xs
    go _ _ _ = error "impossible"

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
