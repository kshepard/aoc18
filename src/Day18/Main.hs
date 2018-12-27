-- | Day 18

{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  )
where

import           Data.List                      ( nub )
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( catMaybes )

data Coord =
  Coord { cx :: !Int
        , cy :: !Int
        } deriving (Eq, Show)

instance Ord Coord where
  (Coord x1 y1) `compare` (Coord x2 y2) = (y1, x1) `compare` (y2, x2)

data Acre = Trees | Lumber | Open deriving (Eq, Ord)

instance Show Acre where
  show = \case
    Trees  -> "|"
    Lumber -> "#"
    Open   -> "."

newtype Area = Area { unArea :: Map Coord Acre } deriving (Eq, Ord)

instance Show Area where
  show (Area area) = unlines rows
    where
      rows   = (\y -> xs >>= (\x -> show (area M.! Coord x y))) <$> ys
      xs     = [minX .. maxX]
      ys     = [minY .. maxY]
      coords = M.keys area
      Coord minX minY = minimum coords
      Coord maxX maxY = maximum coords

parse :: [String] -> Area
parse strLines = Area $ M.fromList tups
 where
  acres     = (fmap . fmap) parseChar strLines
  zipped    = zip [0 ..] $ zip [0 ..] <$> acres
  tups      = zipped >>= (\(y, t) -> (\(x, acre) -> (Coord x y, acre)) <$> t)
  parseChar = \case
    '|' -> Trees
    '#' -> Lumber
    '.' -> Open
    _   -> error "Invalid character"

adjacents :: Coord -> Area -> [Acre]
adjacents (Coord x y) (Area area) = catMaybes
  [ M.lookup (Coord (x + x') (y + y')) area
  | x' <- [-1 .. 1]
  , y' <- [-1 .. 1]
  , not (x' == 0 && y' == 0)
  ]

transformOpen :: [Acre] -> Acre
transformOpen acres =
  if length (filter (== Trees) acres) >= 3 then Trees else Open

transformTrees :: [Acre] -> Acre
transformTrees acres =
  if length (filter (== Lumber) acres) >= 3 then Lumber else Trees

transformLumber :: [Acre] -> Acre
transformLumber acres =
  if length (nub (filter (/= Open) acres)) == 2 then Lumber else Open

transform :: Area -> Area
transform area = Area . M.mapWithKey f $ unArea area
 where
  f c =
    let adj = adjacents c area
    in  \case
          Trees  -> transformTrees adj
          Lumber -> transformLumber adj
          Open   -> transformOpen adj

resourceValue :: Area -> Int
resourceValue (Area area) = length trees * length lumber
 where
  acres  = M.elems area
  trees  = filter (== Trees) acres
  lumber = filter (== Lumber) acres

findCycle :: Area -> (Int, Int, Area)
findCycle = go mempty 1
 where
  go cache n area = case M.lookup area cache of
    Just (area', n') -> (n', n, area')
    Nothing ->
      let area'  = transform area
          cache' = M.insert area (area', n) cache
      in  go cache' (n + 1) area'

part1 :: Area -> Int
part1 area = resourceValue $ iterate transform area !! 10

part2 :: Area -> Int
part2 area =
  resourceValue $ iterate transform area' !! (additional `rem` cycleLength)
 where
  (n1, n2, area') = findCycle area
  cycleLength     = n2 - n1
  additional      = 1000000000 - n2

main :: IO ()
main = do
  area <- parse . lines <$> readFile "input/18.txt"
  print $ part1 area
  print $ part2 area
