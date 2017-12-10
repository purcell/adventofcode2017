module Day10
  ( day10
  ) where

import Data.List (foldl')
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

applyLength :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
applyLength (xs, pos, skip) l = (xs', (pos + skip + l) `mod` nx, skip + 1)
  where
    nx = length xs
    (before, after) = splitAt pos (cycle xs)
    (toReverse, afterReverse) = splitAt l after
    full = before ++ reverse toReverse ++ afterReverse
    overhangLen =
      if l + pos > nx
        then (l + pos) `mod` nx
        else 0
    overhang = take overhangLen (drop nx full)
    xs' = take nx (overhang ++ drop overhangLen full ++ full)

part1 :: [Int] -> Int
part1 =
  product . take 2 . (\(xs, _, _) -> xs) . foldl' applyLength ([0 .. 255], 0, 0)

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* optional newline <* eof) path
  either (error . show) return result

parser :: Parser [Int]
parser = sepBy1 (read <$> many1 digit) (char ',')

day10 :: IO ()
day10 =
  withInput "input/10.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ part1 parsed
