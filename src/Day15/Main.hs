-- | Day 15

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main
  )
where

import           Prelude                 hiding ( round )
import           Data.Foldable                  ( foldl' )
import qualified Data.List                     as L
import qualified Data.List.Key                 as K
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( catMaybes
                                                , isNothing
                                                , mapMaybe
                                                )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )

data Coord =
  Coord { cx :: !Int
        , cy :: !Int
        } deriving (Eq)

instance Ord Coord where
  (Coord x1 y1) `compare` (Coord x2 y2) = (y1, x1) `compare` (y2, x2)

instance Show Coord where
  show Coord {..} = "(" ++ show cx ++ "," ++ show cy ++ ")"

data UnitType = Elf | Goblin deriving (Show, Eq, Ord)

data Unit =
  Unit { unitType    :: !UnitType
       , hitPoints   :: !Int
       , attackPower :: !Int
       , unitId      :: !String
       } deriving (Eq, Ord)

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
    Just ut -> Just (coord, Unit ut 200 3 $ show coord)
  parseChar = \case
    '#' -> (True, Nothing)
    '.' -> (False, Nothing)
    'E' -> (False, Just Elf)
    'G' -> (False, Just Goblin)
    _   -> (False, Nothing)

noWall :: Cave -> Coord -> Bool
noWall Cave {..} coord = S.notMember coord walls

noUnit :: Cave -> Coord -> Bool
noUnit Cave {..} coord = isNothing $ M.lookup coord units

open :: Cave -> Coord -> Bool
open cave coord = noWall cave coord && noUnit cave coord

buildEdges :: Cave -> [(Coord, Coord, Float)]
buildEdges cave = openCoords >>= edges
 where
  openCoords = filter (open cave) $ allCoords cave
  edges c1 = (c1, , 1) <$> openAdjacents cave c1

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

adjToEnemy :: Cave -> (Coord, Unit) -> Bool
adjToEnemy Cave {..} (coord, unit) = or adjacentEnemies
 where
  adjacentUnits   = catMaybes $ (`M.lookup` units) <$> mkAdjacents coord
  adjacentEnemies = enemy unit <$> adjacentUnits

nextCoord :: Cave -> Map Coord [(Coord, Float)] -> Coord -> Maybe Coord
nextCoord cave graph coord =
  if L.null shortestPaths || L.null unitAdjacents || (not . L.null) attackables
    then Nothing
    else Just minCoord
 where
  attackables    = attackable cave coord
  originUnit     = units cave M.! coord
  unitAdjacents  = openAdjacents cave coord
  enemyAdjacents = filter someOpen $ inRange cave originUnit
  someOpen       = not . L.null . openAdjacents cave
  coordPairs     = [ (a1, a2) | a1 <- unitAdjacents, a2 <- enemyAdjacents ]
  paths'         = mapMaybe (\(c1, c2) -> shortestPath c1 c2 graph) coordPairs
  oneAways       = filter (\c -> adjToEnemy cave (c, originUnit)) unitAdjacents
  paths          = paths' <> (pure <$> oneAways)
  shortestLength = minimum $ length <$> paths
  shortestPaths  = filter (\p -> length p == shortestLength) paths
  shortestDest   = minimum $ last <$> shortestPaths
  lastIsShortest p = last p == shortestDest
  minCoord = minimum $ (!! 0) <$> filter lastIsShortest shortestPaths

mkAdjacents :: Coord -> [Coord]
mkAdjacents Coord {..} =
  [Coord cx (cy - 1), Coord cx (cy + 1), Coord (cx - 1) cy, Coord (cx + 1) cy]

allCoords :: Cave -> [Coord]
allCoords Cave {..} = [ Coord x y | x <- [minx .. maxx], y <- [miny .. maxy] ]
 where
  Coord minx miny = minimum walls
  Coord maxx maxy = maximum walls

openAdjacents :: Cave -> Coord -> [Coord]
openAdjacents cave coord = filter (open cave) $ mkAdjacents coord

attackable :: Cave -> Coord -> [Coord]
attackable cave coord = filter isEnemy $ mkAdjacents coord
 where
  isEnemy c = case (M.lookup c (units cave), M.lookup coord (units cave)) of
    (Just unit, Just other) -> enemy unit other
    (_, Nothing) ->
      error $ "Coord: " ++ show coord ++ "Units: " ++ show (units cave)
    _ -> False

enemy :: Unit -> Unit -> Bool
enemy u1 u2 = u1 /= u2 && unitType u1 /= unitType u2

targets :: Cave -> Unit -> [(Coord, Unit)]
targets Cave {..} unit = filter (enemy unit . snd) $ M.toList units

inRange :: Cave -> Unit -> [Coord]
inRange cave unit = targetCoords >>= openAdjacents cave
  where targetCoords = fst <$> targets cave unit

move :: Cave -> (Coord, Unit) -> (Cave, (Coord, Unit))
move cave@Cave {..} (coord, unit) = (cave', cu')
 where
  graph        = buildGraph $ buildEdges cave
  moveTo       = nextCoord cave graph coord
  (cave', cu') = case moveTo of
    Nothing -> (cave, (coord, unit))
    Just c ->
      (cave { units = M.insert c unit $ M.delete coord units }, (c, unit))

attack :: Cave -> (Coord, Unit) -> Cave
attack cave (coord, unit) = cave { units = units' }
 where
  attackables = (\c -> (c, units cave M.! c)) <$> attackable cave coord
  noAttack = L.null attackables
  lowestHp = minimum $ hitPoints . snd <$> attackables
  enemiesWithLowestHp = filter (\(_, u) -> hitPoints u == lowestHp) attackables
  (targetCoord, targetUnit) = minimum enemiesWithLowestHp
  injured = targetUnit { hitPoints = hitPoints targetUnit - attackPower unit }
  removed = M.delete targetCoord $ units cave
  units' | noAttack               = units cave
         | hitPoints injured <= 0 = removed
         | otherwise              = M.insert targetCoord injured removed

turn :: Cave -> (Coord, Unit) -> Cave
turn cave@Cave {..} (coord, unit) = case M.lookup coord units of
  Nothing -> cave
  Just u  -> if unitId u == unitId unit then cavePostAttack else cave
 where
  (cavePostMove, newCu) = move cave (coord, units M.! coord)
  cavePostAttack        = attack cavePostMove newCu

round :: (Cave, Int) -> (Cave, Int)
round (cave, n) = (foldl' turn cave . L.sort . M.toList $ units cave, n + 1)

simulate :: (Cave, Int) -> (UnitType, Int, Int)
simulate = outcome . head . dropWhile active . iterate round
 where
  active (Cave {..}, _) =
    length (L.nub $ unitType . snd <$> M.toList units) > 1
  outcome (cave, n) = (ut cave, n * hp cave, alive cave)
  ut Cave {..} = head $ unitType . snd <$> M.toList units
  hp Cave {..} = sum $ hitPoints . snd <$> M.toList units
  alive Cave {..} = length units

part1 :: Cave -> Int
part1 cave = outcome where (_, outcome, _) = simulate (cave, -1)

part2 :: Cave -> Int
part2 cave = outcome
 where
  (_, outcome, _) = head . dropWhile deaths $ sim . powerUp <$> [4 ..]
  sim cave' = simulate (cave', -1)
  powerUp n = cave { units = M.fromList (poweredUpCus n) }
  poweredUpCus n =
    (\(c, u) -> (c, if unitType u == Elf then u { attackPower = n } else u))
      <$> M.toList (units cave)
  maxElves =
    length . filter (\u -> unitType u == Elf) $ snd <$> M.toList (units cave)
  deaths (_, _, alive) = alive /= maxElves

main :: IO ()
main = do
  cave <- parse . lines <$> readFile "input/15.txt"
  print $ part1 cave
  print $ part2 cave
