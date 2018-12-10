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

height :: [Point] -> Int
height point = maxy - miny where Bbox {..} = mkBbox point

solve :: [Point] -> [(Int, [Point])]
solve = take 1 . filter ((< 10) . height . snd) . zip [0 ..] . iterate step

main :: IO ()
main = do
  points <- parseLines pointParser "input/10.txt"
  mapM_ (\(n, ps) -> displayGrid ps >> print n) $ solve points
