-- | Day 13

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import           Data.List                      ( sort )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as S

data Coord =
  Coord { cx :: !Int
        , cy :: !Int
        } deriving (Show, Eq)

instance Ord Coord where
  (Coord x1 y1) `compare` (Coord x2 y2) = (y1, x1) `compare` (y2, x2)

data Path = Horizontal | Vertical | Curve | Intersection | NoPath
  deriving (Show, Eq)

newtype Track = Track (Coord -> Path)

data Dir = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Eq)

data Turn = TurnLeft | TurnRight | NoTurn
  deriving (Show, Eq)

data Status = Running | Crashed
  deriving (Show, Eq)

data Cart =
  Cart { coord  :: !Coord
       , dir    :: !Dir
       , turn   :: !Turn
       , status :: !Status
       } deriving (Eq, Show)

instance Ord Cart where
  c1 `compare` c2 = coord c1 `compare` coord c2

parseChar :: Char -> (Path, Maybe Dir)
parseChar = \case
  '-'  -> (Horizontal, Nothing)
  '<'  -> (Horizontal, Just DirLeft)
  '>'  -> (Horizontal, Just DirRight)
  '|'  -> (Vertical, Nothing)
  '^'  -> (Vertical, Just DirUp)
  'v'  -> (Vertical, Just DirDown)
  '/'  -> (Curve, Nothing)
  '\\' -> (Curve, Nothing)
  '+'  -> (Intersection, Nothing)
  _    -> (NoPath, Nothing)

parse :: [String] -> (Track, [Cart])
parse strLines = (Track track, carts)
 where
  pathDirs = (fmap . fmap) parseChar strLines
  zipped   = zip [0 ..] $ zip [0 ..] <$> pathDirs
  tups     = zipped >>= (\(y, t) -> (\(x, pd) -> (Coord x y, pd)) <$> t)
  carts    = catMaybes $ maybeCart <$> tups
  pathTups = (\(coord, (path, _)) -> (coord, path)) <$> tups
  track coord = M.findWithDefault NoPath coord $ M.fromList pathTups
  maybeCart (coord, (_, maybeDir)) = case maybeDir of
    Nothing  -> Nothing
    Just dir -> Just $ Cart coord dir TurnLeft Running

adjacents :: Coord -> (Coord, Coord, Coord, Coord)
adjacents coord@Coord {..} =
  ( coord { cy = cy - 1 }
  , coord { cy = cy + 1 }
  , coord { cx = cx - 1 }
  , coord { cx = cx + 1 }
  )

intersectCoordDir :: Cart -> (Coord, Dir)
intersectCoordDir Cart {..} = case (dir, turn) of
  (DirLeft , TurnLeft ) -> (cDown, DirDown)
  (DirLeft , TurnRight) -> (cUp, DirUp)
  (DirLeft , NoTurn   ) -> (cLeft, DirLeft)
  (DirRight, TurnLeft ) -> (cUp, DirUp)
  (DirRight, TurnRight) -> (cDown, DirDown)
  (DirRight, NoTurn   ) -> (cRight, DirRight)
  (DirUp   , TurnLeft ) -> (cLeft, DirLeft)
  (DirUp   , TurnRight) -> (cRight, DirRight)
  (DirUp   , NoTurn   ) -> (cUp, DirUp)
  (DirDown , TurnLeft ) -> (cRight, DirRight)
  (DirDown , TurnRight) -> (cLeft, DirLeft)
  (DirDown , NoTurn   ) -> (cDown, DirDown)
  where (cUp, cDown, cLeft, cRight) = adjacents coord

nextCoordDir :: Track -> Cart -> (Coord, Dir)
nextCoordDir (Track track) cart@Cart {..} = case (track coord, dir) of
  (Horizontal  , DirLeft ) -> (cLeft, DirLeft)
  (Horizontal  , DirRight) -> (cRight, DirRight)
  (Vertical    , DirUp   ) -> (cUp, DirUp)
  (Vertical    , DirDown ) -> (cDown, DirDown)
  (Curve       , DirUp   ) -> vertCurve
  (Curve       , DirDown ) -> vertCurve
  (Curve       , DirLeft ) -> horizCurve
  (Curve       , DirRight) -> horizCurve
  (Intersection, _       ) -> intersectCoordDir cart
  _                        -> error "Invalid path state"
 where
  (cUp, cDown, cLeft, cRight) = adjacents coord
  pDown                       = track cDown
  pLeft                       = track cLeft
  vertCurve = if pLeft == Horizontal || pLeft == Intersection || pLeft == Curve
    then (cLeft, DirLeft)
    else (cRight, DirRight)
  horizCurve = if pDown == Vertical || pDown == Intersection || pDown == Curve
    then (cDown, DirDown)
    else (cUp, DirUp)

move :: Track -> [Cart] -> Cart -> Cart
move t@(Track track) carts cart = case status cart of
  Crashed -> cart
  Running ->
    let
      path           = track $ coord cart
      (coord', dir') = nextCoordDir t cart
      turn'          = case (path, turn cart) of
        (Intersection, TurnLeft ) -> NoTurn
        (Intersection, NoTurn   ) -> TurnRight
        (Intersection, TurnRight) -> TurnLeft
        _                         -> turn cart
      status' =
        if any (\c -> coord c == coord') carts then Crashed else Running
    in
      Cart {coord = coord', dir = dir', turn = turn', status = status'}

crashed :: Cart -> Bool
crashed Cart {..} = status == Crashed

tick :: Track -> Bool -> [Cart] -> [Cart]
tick track removeCrashes carts =
  maybeRemove . snd . foldl' step (S.fromList carts, mempty) . sort $ carts
 where
  step (cartSet, cartsAcc) cart =
    let setWithoutCurr = S.delete cart cartSet
        movedCart      = move track (S.toList setWithoutCurr) cart
        cartSet'       = S.insert movedCart setWithoutCurr
    in  (cartSet', movedCart : cartsAcc)
  crashCoords = fmap coord . filter crashed
  maybeRemove cs = if removeCrashes
    then filter (\c -> coord c `notElem` crashCoords cs) cs
    else cs

part1 :: Track -> [Cart] -> Coord
part1 track =
  coord
    . head
    . filter crashed
    . head
    . take 1
    . dropWhile (not . any crashed)
    . iterate (tick track False)

part2 :: Track -> [Cart] -> Coord
part2 track =
  coord . head . head . take 1 . dropWhile (\cs -> length cs > 1) . iterate
    (tick track True)

main :: IO ()
main = do
  (track, carts) <- parse . lines <$> readFile "input/13.txt"
  print $ part1 track carts
  print $ part2 track carts
