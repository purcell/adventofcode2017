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

runInput :: ([Int] -> Int) -> IO ()
runInput f = do
  result <- parseFromFile (many1 (digitToInt <$> digit)) "input/1.txt"
  either (error . show) (print . f) result

day01 :: IO ()
day01 = do
  putStrLn "Part 1"
  runInput (checksum 1)
  putStrLn "Part 2"
  runInput checksumHalfWay
