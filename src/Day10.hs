module Day10
  ( day10
  ) where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Numeric (showHex)
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

applyLengths :: [Int] -> [Int]
applyLengths = fst3 . foldl' applyLength ([0 .. 255], 0, 0)
  where
    fst3 (xs, _, _) = xs

part1 :: [Int] -> Int
part1 = product . take 2 . applyLengths

part2 :: String -> String
part2 xs = concatMap (pad . (`showHex` "")) denseHash
  where
    pad [c] = ['0', c]
    pad cs = cs
    bytes = ord <$> xs
    denseHash = blockVal <$> blocks
    blockVal b = foldr xor (head b) (tail b)
    blocks = take 16 <$> chunksOf 16 sparseHash
    sparseHash =
      applyLengths (concat (replicate 64 (bytes ++ [17, 31, 73, 47, 23])))

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* optional newline <* eof) path
  either (error . show) return result

parser :: Parser [Int]
parser = sepBy1 (read <$> many1 digit) (char ',')

day10 :: IO ()
day10 = do
  withInput "input/10.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ part1 parsed
  withInput "input/10.txt" (many1 (satisfy (/= '\n'))) >>= \parsed -> do
    putStrLn "Part 2"
    print $ part2 parsed
