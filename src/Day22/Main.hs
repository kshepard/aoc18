-- | Day 22

{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import qualified Data.List                     as L
import qualified Data.List.Key                 as K
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
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
        } deriving (Eq)

instance Ord Coord where
  (Coord x1 y1) `compare` (Coord x2 y2) = (y1, x1) `compare` (y2, x2)

instance Show Coord where
  show Coord {..} = "(" ++ show cx ++ "," ++ show cy ++ ")"

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
  Cave { unCave :: Map Coord RegionType }

instance Show Cave where
  show (Cave cave) = foldl' f "" cs
    where
      cs      = [ Coord x y | y <- [0..maxy ], x <- [0..maxx] ]
      maxc    = maximum $ M.keys cave
      maxx    = cx maxc
      maxy    = cy maxc
      f acc c = acc ++ show (cave M.! c) ++ (if cx c == maxx then "\n" else "")

data Tool =
  Torch | ClimbingGear | Neither deriving (Eq, Ord)

instance Show Tool where
  show Torch        = "T"
  show ClimbingGear = "C"
  show Neither      = "N"

data CoordWithTool =
  CoordWithTool !Coord !Tool deriving (Eq, Ord)

instance Show CoordWithTool where
  show (CoordWithTool (Coord x y) t) =
    "(" ++ show x ++ "," ++ show y ++ "," ++ show t ++ ")"

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

toolCoords :: Coord -> [CoordWithTool]
toolCoords c =
  [CoordWithTool c Torch, CoordWithTool c ClimbingGear, CoordWithTool c Neither]

adjacentCoords :: Coord -> Cave -> [Coord]
adjacentCoords Coord {..} (Cave cave) = filter
  (`M.member` cave)
  [ Coord cx       cy
  , Coord (cx - 1) cy
  , Coord (cx + 1) cy
  , Coord cx       (cy - 1)
  , Coord cx       (cy + 1)
  ]

mkPair :: CoordWithTool -> CoordWithTool -> (CoordWithTool, CoordWithTool)
mkPair ct1@(CoordWithTool c1 _) ct2@(CoordWithTool c2 _)
  | c1 <= c2  = (ct1, ct2)
  | otherwise = (ct2, ct1)

toolCoordPairs :: Coord -> Cave -> [(CoordWithTool, CoordWithTool)]
toolCoordPairs coord cave =
  [ mkPair c1 c2 | (c1 : cs) <- L.tails coords, c2 <- cs, oneAway c1 c2 ]
 where
  coords = adjacentCoords coord cave >>= toolCoords
  oneAway (CoordWithTool (Coord x1 y1) _) (CoordWithTool (Coord x2 y2) _) =
    abs (x1 - x2) + abs (y1 - y2) == 1

invalidState :: RegionType -> Tool -> Bool
invalidState Rocky  Neither      = True
invalidState Wet    Torch        = True
invalidState Narrow ClimbingGear = True
invalidState _      _            = False

mkEdge
  :: Cave
  -> (CoordWithTool, CoordWithTool)
  -> Maybe (CoordWithTool, CoordWithTool, Float)
mkEdge (Cave cave) (ct1@(CoordWithTool c1 t1), ct2@(CoordWithTool c2 t2))
  | invalidState rt1 t1 || invalidState rt2 t2 = Nothing
  | otherwise = Just (ct1, ct2, minutes)
 where
  rt1          = cave M.! c1
  rt2          = cave M.! c2
  moveMinutes  = if c1 == c2 then 0 else 1
  equipMinutes = if t1 == t2 then 0 else 7
  minutes      = moveMinutes + equipMinutes

coordEdges :: Cave -> Coord -> Set (CoordWithTool, CoordWithTool, Float)
coordEdges cave coord =
  S.fromList . catMaybes $ mkEdge cave <$> toolCoordPairs coord cave

buildEdges :: Cave -> [(CoordWithTool, CoordWithTool, Float)]
buildEdges cave = S.toList $ foldl' f S.empty $ M.keys (unCave cave)
  where f acc c = acc <> coordEdges cave c

buildGraph :: Ord a => [(a, a, Float)] -> Map a [(a, Float)]
buildGraph g =
  M.fromListWith (++) $ g >>= \(a, b, d) -> [(a, [(b, d)]), (b, [(a, d)])]

shortestPath :: (Ord a, Show a) => a -> a -> Map a [(a, Float)] -> Float
shortestPath from to graph = fst $ dijkstra from graph M.! to

dijkstra :: Ord a => a -> Map a [(a, Float)] -> Map a (Float, Maybe a)
dijkstra source graph = f
  (M.fromList
    [ (v, (if v == source then 0 else 1 / 0, Nothing)) | v <- M.keys graph ]
  )
  (M.keys graph)
 where
  f ds [] = ds
  f ds q  = f (foldr relax ds $ graph M.! m) (L.delete m q)
   where
    m = K.minimum (fst . (ds M.!)) q
    relax (e, d) = M.adjust (min (fst (ds M.! m) + d, Just m)) e

part1 :: CaveParams -> Cave -> Int
part1 CaveParams {..} (Cave cave) = getSum $ M.foldMapWithKey f cave
 where
  f (Coord x y) rt = if x > cx target || y > cy target
    then Sum 0
    else case rt of
      Rocky  -> Sum 0
      Wet    -> Sum 1
      Narrow -> Sum 2

part2 :: CaveParams -> Cave -> Int
part2 CaveParams {..} cave = round $ shortestPath start end graph
 where
  graph = buildGraph $ buildEdges cave
  start = CoordWithTool (Coord 0 0) Torch
  end   = CoordWithTool target Torch

main :: IO ()
main = do
  caveParams <- parseCaveParams <$> readFile "input/22.txt"
  let cave = mkCave caveParams
  print $ part1 caveParams cave
  print $ part2 caveParams cave
