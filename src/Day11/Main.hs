-- | Day 11

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import qualified Data.Map.Strict               as M

size :: Int
size = 300

power :: Int -> Int -> Int -> Int
power x y serial = (rack * y + serial) * rack `div` 100 `mod` 10 - 5
  where rack = x + 10

powerSum :: Int -> Int -> M.Map (Int, Int) Int -> Int -> Int
powerSum x y cells sqSize = if (x > size - sqMax) || (y > size - sqMax)
  then 0
  else sum $ (M.!) cells <$> points
 where
  points = [ (x' + x, y' + y) | x' <- [0 .. sqMax], y' <- [0 .. sqMax] ]
  sqMax  = sqSize - 1

mkCells :: Int -> M.Map (Int, Int) Int
mkCells serial =
  M.fromList [ ((x, y), power x y serial) | x <- [1 .. size], y <- [1 .. size] ]

part1 :: M.Map (Int, Int) Int -> Int -> ((Int, Int), Int)
part1 cells sqSize = foldl' step ((0, 0), 0) $ M.toList cells
 where
  step acc@((_, _), maxPow) ((x, y), _) =
    let ps = powerSum x y cells sqSize
    in  if ps > maxPow then ((x, y), ps) else acc

part2 :: M.Map (Int, Int) Int -> (((Int, Int), Int), Int)
part2 cells = foldl' step (((0, 0), 0), 0) [1 .. 20]
 where
  step acc@(((_, _), maxPow), _) sqSize =
    let p1@((_, _), pow) = part1 cells sqSize
    in  if pow > maxPow then (p1, sqSize) else acc

main :: IO ()
main = do
  raw <- init <$> readFile "input/11.txt"
  let serial = read raw :: Int
      cells  = mkCells serial
  print $ part1 cells 3
  print $ part2 cells
