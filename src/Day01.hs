module Day01
    ( day01
    ) where

import Text.Parsec
import Text.Parsec.String


checksum :: (Eq a, Num a) => [a] -> a
checksum [] = 0
checksum ds@(d:_) = sum [d1 | (d1, d2) <- pairs (ds ++ [d]), d1 == d2]
  where pairs :: [a] -> [(a,a)]
        pairs xs = zip xs (tail xs)


day01 :: IO ()
day01 = do
  result <- parseFromFile (many1 numericDigit) "input/1.txt"
  either (error . show) (print . checksum) result
  where numericDigit :: Parser Int
        numericDigit = digit >>= \d -> return (read [d])
