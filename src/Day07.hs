module Day07
  ( day07
  ) where

import Control.Arrow ((&&&))
import Data.Maybe (listToMaybe)
import Data.Tree (Tree(..))
import qualified Data.Tree as T
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

data Program = Program
  { tName :: String
  , tWeight :: Int
  , tNamesAbove :: [String]
  } deriving (Show)

towers :: [Program] -> Tree (String, Int)
towers ps = T.unfoldTree build root
  where
    build name =
      let (Program n w above) = findByName name
      in ((n, w), above)
    findByName n = head (filter ((n ==) . tName) ps)
    root = tName (head (filter hasNoParent ps))
    hasNoParent p = null [p' | p' <- ps, tName p `elem` tNamesAbove p']

highestUnbalanced :: Tree (String, Int) -> Int -> Maybe (String, Int, Int)
highestUnbalanced tree delta =
  maybe checkYoSelf (uncurry highestUnbalanced) (oddSubtree tree)
  where
    checkYoSelf =
      if delta == 0
        then Nothing
        else let (Node (s, w) _) = tree
             in Just (s, w, w + delta)

oddSubtree :: Tree (String, Int) -> Maybe (Tree (String, Int), Int)
oddSubtree (Node _ above) =
  listToMaybe
    [ (n, bestWeight - w)
    | (n, w) <- withWeights
    , bestWeight <- [w' | w' <- weights, length (filter (/= w') weights) <= 1]
    , w /= bestWeight
    ]
  where
    withWeights = map (id &&& totalWeight) above
    weights = map snd withWeights

totalWeight :: Tree (String, Int) -> Int
totalWeight t = sum (snd <$> T.flatten t)

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

parser :: Parser [Program]
parser = many1 (tower <* newline)
  where
    tower =
      Program <$> (name <* char ' ') <*> (char '(' *> number <* char ')') <*>
      option [] nameList
    nameList = string " -> " *> sepBy1 name (string ", ")
    name = many1 lower
    number = read <$> many1 digit

day07 :: IO ()
day07 =
  withInput "input/7.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ head (T.flatten (towers parsed))
    putStrLn "Part 2"
    print $ highestUnbalanced (towers parsed) 0
