module Day09
  ( day09
  ) where

import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* optional newline <* eof) path
  either (error . show) return result

data Content
  = Garbage Int
  | Group [Content]
  deriving (Show)

totalScore :: [Content] -> Int
totalScore = sum . fmap (score 1)
  where
    score _ (Garbage _) = 0
    score level (Group contained) =
      level + sum (score (level + 1) <$> contained)

garbageLength :: [Content] -> Int
garbageLength = sum . fmap glen
  where
    glen (Garbage l) = l
    glen (Group contained) = garbageLength contained

parser :: Parser [Content]
parser = many content
  where
    content = garbage <|> group
    garbage, group :: Parser Content
    garbage = Garbage <$> (char '<' *> (sum <$> garbageContents) <* char '>')
    garbageContents =
      many ((char '!' *> anyChar $> 0) <|> (satisfy (`notElem` "!>") $> 1))
    group = Group <$> (char '{' *> sepBy content (char ',') <* char '}')

day09 :: IO ()
day09 =
  withInput "input/9.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ totalScore parsed
    putStrLn "Part 2"
    print $ garbageLength parsed
