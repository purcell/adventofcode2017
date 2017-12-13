module Day12
  ( day12
  ) where

import Control.Arrow ((&&&))
import Data.Graph.Inductive.Basic (undir)
import Data.Graph.Inductive.Graph (LNode, mkGraph)
import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS (bfs)
import qualified Data.Set as S
import Data.Set ((\\))
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

type Network = Gr Int ()

makeGraph :: [Pipe] -> Network
makeGraph pipes = undir $ mkGraph nodes edges
  where
    nodes = toNode <$> (pFrom <$> pipes) ++ concatMap pTo pipes
    edges = [(f, t, ()) | (Pipe f ts) <- pipes, t <- ts]

toNode :: Int -> LNode Int
toNode = id &&& id

subgraphCount :: Network -> Int
subgraphCount g = go 0 (S.fromList (G.nodes g))
  where
    go n unseen
      | S.null unseen = n
    go n unseen =
      go (n + 1) (unseen \\ S.fromList (bfs (head (S.toList unseen)) g))

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* optional newline <* eof) path
  either (error . show) return result

parser :: Parser [Pipe]
parser = many1 (pipe <* newline)
  where
    pipe = Pipe <$> number <*> (string " <-> " *> sepBy1 number (string ", "))
    number = read <$> many1 digit

data Pipe = Pipe
  { pFrom :: Int
  , pTo :: [Int]
  }

day12 :: IO ()
day12 =
  withInput "input/12.txt" parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ length $ bfs 0 $ makeGraph parsed
    putStrLn "Part 2"
    print $ subgraphCount $ makeGraph parsed
