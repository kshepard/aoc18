-- | Day 15

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )

data Coord =
  Coord { cx :: !Int
        , cy :: !Int
        } deriving (Show, Eq)

instance Ord Coord where
  (Coord x1 y1) `compare` (Coord x2 y2) = (y1, x1) `compare` (y2, x2)

data UnitType = Elf | Goblin deriving (Show, Eq)

data Unit =
  Unit { unitType    :: !UnitType
       , hitPoints   :: !Int
       , attackPower :: !Int
       }

instance Show Unit where
  show Unit {..} = ut ++ "(" ++ show hitPoints ++ ")"
    where
      ut = if unitType == Elf then "E" else "G"

data Cave =
  Cave { walls :: Set Coord
       , units :: Map Coord Unit
       }

instance Show Cave where
  show Cave {..} = concat $ showRow <$> [miny .. maxy]
    where
      Coord minx miny = minimum walls
      Coord maxx maxy = maximum walls
      showRow y       = concat (showCell y <$> [minx .. maxx])
        ++ showUnits y ++ "\n"
      showCell y x    = case M.lookup (Coord x y) units of
        Nothing -> if S.member (Coord x y) walls then "#" else "."
        Just Unit {..} -> if unitType == Elf then "E" else "G"
      showUnits y = M.foldlWithKey (fsu y) "  " units
      fsu y acc Coord {..} u = if cy == y
        then acc ++ show u ++ "  "
        else acc

parse :: [String] -> Cave
parse strLines = Cave walls units
 where
  wallUnits = (fmap . fmap) parseChar strLines
  zipped    = zip [0 ..] $ zip [0 ..] <$> wallUnits
  tups      = zipped >>= (\(y, t) -> (\(x, wu) -> (Coord x y, wu)) <$> t)
  units     = M.fromList . catMaybes $ maybeUnit <$> tups
  walls     = S.fromList $ fst <$> filter (\(_, (w, _)) -> w) tups
  maybeUnit (coord, (_, maybeUt)) = case maybeUt of
    Nothing -> Nothing
    Just ut -> Just (coord, Unit ut 200 3)
  parseChar = \case
    '#' -> (True, Nothing)
    '.' -> (False, Nothing)
    'E' -> (False, Just Elf)
    'G' -> (False, Just Goblin)
    _   -> (False, Nothing)

main :: IO ()
main = do
  cave <- parse . lines <$> readFile "input/15.txt"
  print cave
