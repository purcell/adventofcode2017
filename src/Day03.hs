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
sumValues = fst <$> scanl totalAt (1, M.singleton startPos 1) rest
  where
    (startPos:rest) = positions
    totalAt (_, sums) pos = (thisVal, M.insert pos thisVal sums)
      where
        thisVal =
          sum . catMaybes . fmap (`M.lookup` sums) . neighbourPositions $ pos

neighbourPositions :: Pos -> [Pos]
neighbourPositions (x, y) =
  [ (x', y')
  | x' <- [x - 1, x, x + 1]
  , y' <- [y - 1, y, y + 1]
  , (x, y) /= (x', y')
  ]

day03 :: IO ()
day03 = do
  putStrLn "Part 1"
  let (x, y) = positionOf 347991
  print $ abs x + abs y
  putStrLn "Part 2"
  print $ head $ dropWhile (<= 347991) sumValues
