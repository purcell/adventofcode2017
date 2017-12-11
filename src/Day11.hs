module Day11
  ( day11
  ) where

import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

data GridPos = GridPos
  { gx :: Int
  , gy :: Int
  , gz :: Int
  }

origin :: GridPos
origin = GridPos 0 0 0

data Move
  = N
  | NE
  | SE
  | S
  | SW
  | NW
  | E
  | W

move :: Move -> GridPos -> GridPos
move m (GridPos x y z) =
  case m of
    N -> GridPos x (y + 1) (z - 1)
    S -> GridPos x (y - 1) (z + 1)
    NE -> GridPos (x + 1) y (z - 1)
    SW -> GridPos (x - 1) y (z + 1)
    NW -> GridPos (x - 1) (y + 1) z
    SE -> GridPos (x + 1) (y - 1) z

distanceBetween :: GridPos -> GridPos -> Int
distanceBetween (GridPos x y z) (GridPos x' y' z') =
  (abs (x - x') + abs (y - y') + abs (z - z')) `div` 2

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* optional newline <* eof) path
  either (error . show) return result

parser :: Parser [Move]
parser = sepBy1 parseMove (char ',')
  where
    parseMove =
      try (string "ne" $> NE) <|> try (string "nw" $> NW) <|>
      try (string "sw" $> SW) <|>
      try (string "se" $> SE) <|>
      try (string "n" $> N) <|>
      try (string "s" $> S) <|>
      try (string "e" $> E) <|>
      try (string "w" $> W)

day11 :: IO ()
day11 = do
  withInput "input/11.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ distanceBetween origin (foldr move origin parsed)
