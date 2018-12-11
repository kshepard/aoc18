-- | Day 06

{-# LANGUAGE TupleSections #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import           Data.List                      ( maximumBy
                                                , nub
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe )
import           Data.Ord                       ( comparing )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( eol
                                                , string
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Parser = Parsec Void String

data Coord =
  Coord { cx :: !Int
        , cy :: !Int
        } deriving (Show, Eq, Ord)

data Bbox =
  Bbox { nw :: !Coord
       , se :: !Coord
       } deriving (Show)

newtype Grid a = Grid (Coord -> a)

coordParser :: Parser Coord
coordParser = do
  x <- decimal
  string ", "
  y <- decimal
  pure $ Coord {cx = x, cy = y}

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> return x

mkBbox :: [Coord] -> Bbox
mkBbox coords = Bbox
  { nw = Coord {cx = minimum xs - 1, cy = minimum ys - 1}
  , se = Coord {cx = maximum xs + 1, cy = maximum ys + 1}
  }
 where
  xs = cx <$> coords
  ys = cy <$> coords

coordsInBbox :: Bbox -> [Coord]
coordsInBbox (Bbox (Coord x1 y1) (Coord x2 y2)) =
  [ Coord x y | x <- [x1 .. x2], y <- [y1 .. y2] ]

edgeCoords :: Bbox -> [Coord] -> [Coord]
edgeCoords (Bbox (Coord x1 y1) (Coord x2 y2)) =
  filter (\(Coord x y) -> x == x1 || x == x2 || y == y1 || y == y2)

distance :: Coord -> Coord -> Int
distance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

distances :: Coord -> [Coord] -> [(Coord, Int)]
distances coord = fmap (\c -> (c, distance c coord))

closest :: Coord -> [Coord] -> Maybe Coord
closest coord places = if length minMatches == 1
  then Just . fst . head $ minMatches
  else Nothing
 where
  dists       = distances coord places
  minDistance = minimum $ snd <$> dists
  minMatches  = filter (\(_, d) -> d == minDistance) dists

sumDistances :: Coord -> [Coord] -> Int
sumDistances coord = sum . fmap snd . distances coord

findInfinites :: Grid (Maybe Coord) -> [Coord] -> [Coord]
findInfinites (Grid g) = nub . mapMaybe g

frequencyMap :: Ord a => [a] -> M.Map a Int
frequencyMap = M.fromListWith (+) . fmap (, 1)

part1 :: [Coord] -> Int
part1 places =
  snd
    . maximumBy (comparing snd)
    . filter (\(c, _) -> notElem c infinites)
    $ areas
 where
  bbox          = mkBbox places
  allCoords     = coordsInBbox bbox
  infinites     = findInfinites grid $ edgeCoords bbox allCoords
  areas         = M.toList . frequencyMap . mapMaybe g $ allCoords
  grid@(Grid g) = foldl' updateGrid (Grid $ const Nothing) allCoords
  updateGrid (Grid accFn) coord =
    Grid $ \c -> if c == coord then closest c places else accFn c

part2 :: [Coord] -> Int
part2 places = length $ filter (\c -> g c < 10000) allCoords
 where
  bbox      = mkBbox places
  allCoords = coordsInBbox bbox
  (Grid g)  = foldl' updateGrid (Grid $ const 0) allCoords
  updateGrid (Grid accFn) coord =
    Grid $ \c -> if c == coord then sumDistances c places else accFn c

main :: IO ()
main = do
  places <- parseLines coordParser "input/06.txt"
  print $ part1 places
  print $ part2 places
