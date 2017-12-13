module Day13
  ( day13
  ) where

import Data.Maybe (listToMaybe)
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

severity :: [Layer] -> Int -> Int
severity ls t = maybe 0 sev layer
  where
    sev l =
      if scannerPos l t == 0
        then lDepth l * lRange l
        else 0
    playerPos = t
    layer = listToMaybe [l | l <- ls, lDepth l == playerPos]

scannerPos :: Layer -> Int -> Int
scannerPos l t =
  if odd d
    then mx - m
    else m
  where
    mx = lRange l - 1
    (d, m) = t `divMod` mx

maxDepth :: [Layer] -> Int
maxDepth ls = maximum (lDepth <$> ls)

tripSeverity :: [Layer] -> Int
tripSeverity ls = sum (severity ls <$> [0 .. maxDepth ls])

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* optional newline <* eof) path
  either (error . show) return result

data Layer = Layer
  { lDepth :: Int
  , lRange :: Int
  }

parser :: Parser [Layer]
parser = many1 (layer <* newline)
  where
    layer = Layer <$> number <*> (string ": " *> number)
    number = read <$> many1 digit

example = [Layer 0 3, Layer 1 2, Layer 4 4, Layer 6 4]

test = tripSeverity example

day13 :: IO ()
day13 =
  withInput "input/13.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ tripSeverity parsed
