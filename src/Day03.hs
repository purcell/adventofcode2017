{-# LANGUAGE TupleSections #-}

module Day03
  ( day03
  ) where

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S

data Direction
  = N
  | E
  | S
  | W

type Pos = (Int, Int)

turnLeft :: Direction -> Direction
turnLeft N = W
turnLeft W = S
turnLeft S = E
turnLeft E = N

move :: Direction -> Pos -> Pos
move N (x, y) = (x, y + 1)
move S (x, y) = (x, y - 1)
move E (x, y) = (x + 1, y)
move W (x, y) = (x - 1, y)

positionOf :: Int -> Pos
positionOf n = positions !! (n - 1)

positions :: [Pos]
positions = (\(p, _, _) -> p) <$> iterate next ((0, 0), S, S.singleton (0, 0))
  where
    next (pos, dir, seen) =
      if leftPos `S.notMember` seen
        then (leftPos, left, S.insert leftPos seen)
        else (move dir pos, dir, S.insert (move dir pos) seen)
      where
        leftPos = move left pos
        left = turnLeft dir

sumValues :: [Int]
sumValues = go M.empty positions
  where
    go _ [] = []
    go sums ((0, 0):rest) = 1 : go (M.insert (0, 0) 1 sums) rest
    go sums (pos:rest) = thisVal : go (M.insert pos thisVal sums) rest
      where
        thisVal =
          sum . catMaybes . fmap (`M.lookup` sums) . neighbourPositions $ pos

neighbourPositions :: Pos -> [Pos]
neighbourPositions (x, y) =
  [ pos
  | dx <- [-1 .. 1]
  , dy <- [-1 .. 1]
  , let pos = (x + dx, y + dy)
  , pos /= (x, y)
  ]

day03 :: IO ()
day03 = do
  putStrLn "Part 1"
  let (x, y) = positionOf 347991
  print $ abs x + abs y
  putStrLn "Part 2"
  print $ head $ dropWhile (<= 347991) sumValues
