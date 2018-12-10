-- | Day 10

{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( for_ )
import qualified Data.List                     as L
import           Data.Ord                       ( comparing )
import qualified Data.Set                      as S
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( char
                                                , eol
                                                , string
                                                )
import           Text.Megaparsec.Char.Lexer     ( signed
                                                , decimal
                                                )

type Parser = Parsec Void String

data Point =
  Point { px :: !Int
        , py :: !Int
        , vx :: !Int
        , vy :: !Int
        } deriving (Show)

data Bbox =
  Bbox { minx :: !Int
       , miny :: !Int
       , maxx :: !Int
       , maxy :: !Int
       } deriving (Show)

pointParser :: Parser Point
pointParser = do
  string "position=<"
  many $ char ' '
  px <- signed mempty decimal
  string ", "
  many $ char ' '
  py <- signed mempty decimal
  string "> velocity=<"
  many $ char ' '
  vx <- signed mempty decimal
  string ", "
  many $ char ' '
  vy <- signed mempty decimal
  char '>'
  pure Point {..}

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> pure x

coordSet :: [Point] -> S.Set (Int, Int)
coordSet = S.fromList . fmap (\Point {..} -> (px, py))

mkBbox :: [Point] -> Bbox
mkBbox points = Bbox {..}
 where
  minx = px $ L.minimumBy (comparing px) points
  miny = py $ L.minimumBy (comparing py) points
  maxx = px $ L.maximumBy (comparing px) points
  maxy = py $ L.maximumBy (comparing py) points

displayGrid :: [Point] -> IO ()
displayGrid points = do
  let coords    = coordSet points
      Bbox {..} = mkBbox points
  for_ [miny .. maxy] $ \y -> do
    for_ [minx .. maxx]
      $ \x -> putStr $ if S.member (x, y) coords then "#" else "."
    putStrLn ""

step :: [Point] -> [Point]
step =
  fmap (\Point {..} -> Point {px = px + vx, py = py + vy, vx = vx, vy = vy})

area :: [Point] -> Int
area point = maxx - minx * maxy - miny where Bbox {..} = mkBbox point

solve :: [Point] -> ([[Point]], Int)
solve = go (maxBound :: Int) mempty 0
 where
  go prevArea prevPoints count points = if newArea > prevArea
    then
      ( [prevPoints, points, newPoints, step newPoints, (step . step) newPoints]
      , count
      )
    else go newArea points (count + 1) newPoints
   where
    newPoints = step points
    newArea   = area points

main :: IO ()
main = do
  points <- parseLines pointParser "input/10.txt"
  let (grids, count) = solve points
  mapM_ displayGrid grids
  print count
