-- | Day 22

{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( Sum(..)
                                                , getSum
                                                )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( char
                                                , eol
                                                , string
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Parser = Parsec Void String

data Coord =
  Coord { cx :: !Int
        , cy :: !Int
        } deriving (Eq, Show)

instance Ord Coord where
  (Coord x1 y1) `compare` (Coord x2 y2) = (y1, x1) `compare` (y2, x2)

data CaveParams =
  CaveParams { depth  :: !Int
             , target :: !Coord
             }

data RegionType =
  Rocky | Wet | Narrow deriving (Eq, Ord)

instance Show RegionType where
  show Rocky  = "."
  show Wet    = "="
  show Narrow = "|"

newtype Cave =
  Cave (Map Coord RegionType)

instance Show Cave where
  show (Cave cave) = foldl' f "" cs
    where
      cs      = [ Coord x y | y <- [0..maxy ], x <- [0..maxx] ]
      maxc    = maximum $ M.keys cave
      maxx    = cx maxc
      maxy    = cy maxc
      f acc c = acc ++ show (cave M.! c) ++ (if cx c == maxx then "\n" else "")

coordParser :: Parser Coord
coordParser = do
  cx <- decimal
  cy <- char ',' *> decimal
  pure Coord {..}

caveParamsParser :: Parser CaveParams
caveParamsParser = do
  depth  <- string "depth: " *> decimal <* eol
  target <- string "target: " *> coordParser <* eol <* eof
  pure CaveParams {..}

parseCaveParams :: String -> CaveParams
parseCaveParams input = case parse caveParamsParser mempty input of
  Left  err -> error $ parseErrorPretty err
  Right x   -> x

mkCave :: CaveParams -> Cave
mkCave CaveParams {..} = Cave $ rt <$> foldl' f M.empty coords
 where
  coords =
    [ Coord x y | y <- [0 .. (cy target + 5)], x <- [0 .. (cx target + 5)] ]
  rt level | level `mod` 3 == 0 = Rocky
           | level `mod` 3 == 1 = Wet
           | otherwise          = Narrow
  f acc coord = M.insert coord (el coord) acc
   where
    gi c | c == Coord 0 0 = 0
         | c == target    = 0
         | cy c == 0      = 16807 * cx c
         | cx c == 0      = 48271 * cy c
         | otherwise      = el c { cx = cx c - 1 } * el c { cy = cy c - 1 }
    el c = fromMaybe ((gi c + depth) `mod` 20183) $ M.lookup c acc

part1 :: CaveParams -> Cave -> Int
part1 CaveParams {..} (Cave cave) = getSum $ M.foldMapWithKey f cave
 where
  f (Coord x y) rt = if x > cx target || y > cy target
    then Sum 0
    else case rt of
      Rocky  -> Sum 0
      Wet    -> Sum 1
      Narrow -> Sum 2

main :: IO ()
main = do
  caveParams <- parseCaveParams <$> readFile "input/22.txt"
  let cave = mkCave caveParams
  print $ part1 caveParams cave
