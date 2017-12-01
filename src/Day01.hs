module Day01
  ( day01
  ) where

import Data.Char
import Text.Parsec
import Text.Parsec.String

checksum :: (Eq a, Num a) => Int -> [a] -> a
checksum _ [] = 0
checksum offset ds = sum [d1 | (d1, d2) <- pairs ds, d1 == d2]
  where
    pairs :: [a] -> [(a, a)]
    pairs xs = zip xs (drop offset (xs ++ xs))

checksumHalfWay :: [Int] -> Int
checksumHalfWay ds = checksum (length ds `div` 2) ds

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

day01 :: IO ()
day01 =
  withInput "input/1.txt" (many1 (digitToInt <$> digit) <* newline) >>= \digits -> do
    putStrLn "Part 1"
    print (checksum 1 digits)
    putStrLn "Part 2"
    print (checksumHalfWay digits)
