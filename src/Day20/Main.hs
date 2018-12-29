-- | Day 20

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import qualified Data.List                     as L
import qualified Data.List.Key                 as K
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )

data Coord =
  Coord { cx :: !Int
        , cy :: !Int
        } deriving (Eq, Ord, Show)

nextCoord :: Char -> Coord -> Coord
nextCoord = \case
  'N' -> \c@Coord {..} -> c { cy = cy + 1 }
  'S' -> \c@Coord {..} -> c { cy = cy - 1 }
  'E' -> \c@Coord {..} -> c { cx = cx + 1 }
  'W' -> \c@Coord {..} -> c { cx = cx - 1 }
  _   -> error "Invalid direction"

mkConnection :: Coord -> Coord -> (Coord, Coord)
mkConnection c1 c2 | c1 <= c2  = (c1, c2)
                   | otherwise = (c2, c1)

mkConnections :: String -> Set (Coord, Coord)
mkConnections regex = go regex (Coord 0 0) [] S.empty
 where
  go ('^' : rs) c stack             cs = go rs c stack cs
  go ('$' : _ ) _ _                 cs = cs
  go ('(' : rs) c stack             cs = go rs c (c : stack) cs
  go ('|' : rs) _ stack@(c : _    ) cs = go rs c stack cs
  go (')' : rs) _ (      c : stack) cs = go rs c stack cs
  go (d : rs) c stack cs =
    let c' = nextCoord d c in go rs c' stack $ S.insert (mkConnection c c') cs
  go _ _ _ _ = error "Invalid regex"

buildEdges :: Set (Coord, Coord) -> [(Coord, Coord, Float)]
buildEdges = fmap (\(c1, c2) -> (c1, c2, 1)) . S.toList

buildGraph :: Ord a => [(a, a, Float)] -> Map a [(a, Float)]
buildGraph g =
  M.fromListWith (++) $ g >>= \(a, b, d) -> [(a, [(b, d)]), (b, [(a, d)])]

shortestPath :: (Ord a, Show a) => a -> a -> Map a [(a, Float)] -> Maybe [a]
shortestPath from to graph = finalize $ f to
 where
  f x = x : maybe [] f (snd $ dijkstra from graph M.! x)
  finalize path =
    let reversed = reverse path
    in  if head reversed == from then Just reversed else Nothing

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

doorsAway :: Map Coord [(Coord, Float)] -> Coord -> Int
doorsAway graph c = case shortestPath (Coord 0 0) c graph of
  Nothing -> 0
  Just cs -> length cs - 1

main :: IO ()
main = do
  input <- init <$> readFile "input/20.txt"
  let connections = mkConnections input
      graph       = buildGraph $ buildEdges connections
      lengths     = fmap (doorsAway graph) (M.keys graph)
  print $ maximum lengths
  print . length $ filter (>= 1000) lengths
