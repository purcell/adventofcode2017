module Day05
  ( day05
  ) where

import qualified Data.Sequence as Seq
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

jumpsToExit :: (Int -> Int) -> [Int] -> Int
jumpsToExit modifier offsets =
  length $ takeWhile inside $ iterate jump (Seq.fromList offsets, 0)
  where
    inside (instrs, n) = n >= 0 && n < Seq.length instrs
    jump (instrs, n) = (instrs', n')
      where
        n' = n + atN
        instrs' = Seq.update n (modifier atN) instrs
        atN = Seq.index instrs n

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

parser :: Parser [Int]
parser = many1 (number <* newline)
  where
    number = do
      sign <- option '+' (char '-')
      digits <- many1 digit
      return $
        (if sign == '-'
           then -1
           else 1) *
        read digits

day05 :: IO ()
day05 =
  withInput "input/5.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ jumpsToExit (+ 1) parsed
    putStrLn "Part 2"
    print $
      jumpsToExit
        (\off ->
           if off >= 3
             then off - 1
             else off + 1)
        parsed
