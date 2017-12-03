module Day03
  ( day03
  ) where

import qualified Data.Set as S

data Direction
  = N
  | E
  | S
  | W

positionOf :: Int -> (Int, Int)
positionOf n = positions !! (n - 1)

positions :: [(Int, Int)]
positions = (\(p, _, _) -> p) <$> iterate next ((0, 0), S, S.singleton (0, 0))
  where
    next ((x, y), dir, seen) =
      if leftPos `S.notMember` seen
        then (leftPos, left, S.insert leftPos seen)
        else (move dir, dir, S.insert (move dir) seen)
      where
        leftPos = move left
        left = turnLeft dir
        turnLeft N = W
        turnLeft W = S
        turnLeft S = E
        turnLeft E = N
        move N = (x, y + 1)
        move S = (x, y - 1)
        move E = (x + 1, y)
        move W = (x - 1, y)

day03 :: IO ()
day03 = do
  putStrLn "Part 1"
  let (x, y) = positionOf 347991
  print $ abs x + abs y
