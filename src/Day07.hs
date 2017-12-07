module Day07
  ( day07
  ) where

import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

data Program = Program
  { tName :: String
  , tWeight :: Int
  , tNamesAbove :: S.Set String
  } deriving (Show)

bottom :: [Program] -> Maybe Program
bottom programs = listToMaybe [p | p <- programs, hasNoParent p]
  where
    hasNoParent p =
      null [p' | p' <- programs, tName p `S.member` tNamesAbove p']

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

parser :: Parser [Program]
parser = many1 (tower <* newline)
  where
    tower =
      Program <$> (name <* char ' ') <*> (char '(' *> number <* char ')') <*>
      (S.fromList <$> option [] nameList)
    nameList = string " -> " *> sepBy1 name (string ", ")
    name = many1 lower
    number = read <$> many1 digit

day07 :: IO ()
day07 =
  withInput "input/7.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ bottom parsed
