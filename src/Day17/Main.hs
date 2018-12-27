-- | Day 17

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main
  )
where

import           Control.Monad.State.Strict     ( State
                                                , execState
                                                , gets
                                                , modify'
                                                , state
                                                )
import           Data.Foldable                  ( foldl' )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                , (<|>)
                                                )
import           Text.Megaparsec.Char           ( eol
                                                , string
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Parser = Parsec Void String

data Coord =
  Coord { cx :: !Int
        , cy :: !Int
        } deriving (Eq)

instance Ord Coord where
  Coord x1 y1 `compare` Coord x2 y2 = (y1, x1) `compare` (y2, x2)

data Bbox =
  Bbox { minx :: !Int
       , miny :: !Int
       , maxx :: !Int
       , maxy :: !Int
       }

newtype Clay = Clay { unClay :: Set Coord }

newtype Flowing = Flowing { unFlowing :: Set Coord }
  deriving (Semigroup, Monoid)

newtype Settled = Settled { unSettled :: Set Coord }
  deriving (Semigroup, Monoid)

data Dir = DirLeft | DirRight | DirDown
  deriving (Eq)

data Water =
  Water { flowing :: !Flowing
        , settled :: !Settled
        }

horizontalParser :: Parser [Coord]
horizontalParser = do
  y  <- string "y=" *> decimal
  x1 <- string ", x=" *> decimal
  x2 <- string ".." *> decimal
  pure $ (`Coord` y) <$> [x1 .. x2]

verticalParser :: Parser [Coord]
verticalParser = do
  x  <- string "x=" *> decimal
  y1 <- string ", y=" *> decimal
  y2 <- string ".." *> decimal
  pure $ Coord x <$> [y1 .. y2]

lineParser :: Parser [Coord]
lineParser = horizontalParser <|> verticalParser

parseClay :: String -> Clay
parseClay input = case parse (many (lineParser <* eol) <* eof) mempty input of
  Left  err -> error $ parseErrorPretty err
  Right x   -> Clay . S.fromList . concat $ x

clayCoords :: Clay -> [Coord]
clayCoords = S.toList . unClay

isClay :: Coord -> Clay -> Bool
isClay coord = S.member coord . unClay

notClay :: Coord -> Clay -> Bool
notClay = (not .) . isClay

isFlowing :: Coord -> Flowing -> Bool
isFlowing coord = S.member coord . unFlowing

notFlowing :: Coord -> Flowing -> Bool
notFlowing = (not .) . isFlowing

isSettled :: Coord -> Settled -> Bool
isSettled coord = S.member coord . unSettled

notSettled :: Coord -> Settled -> Bool
notSettled = (not .) . isSettled

addFlowing :: Coord -> Flowing -> Flowing
addFlowing coord = Flowing . S.insert coord . unFlowing

addSettled :: Coord -> Settled -> Settled
addSettled coord = Settled . S.insert coord . unSettled

addCoord :: Int -> Int -> Coord -> Coord
addCoord x y Coord {..} = Coord (cx + x) (cy + y)

bbox :: Clay -> Bbox
bbox clay = Bbox {..}
 where
  coords       = clayCoords clay
  (xs  , ys  ) = (cx <$> coords, cy <$> coords)
  (minx, maxx) = (minimum xs - 1, maximum xs + 1)
  (miny, maxy) = (minimum ys, maximum ys)

settleRow :: Water -> Coord -> Water
settleRow st coord = foldl' step st coords
 where
  xs     = mkCoords 1 ++ mkCoords (-1)
  coords = (\x -> Coord (cx coord + x) (cy coord)) <$> xs
  step s c = s { settled = addSettled c (settled s) }
  mkCoord x = Coord (cx coord + x) (cy coord)
  mkCoords n = (* n)
    <$> takeWhile (\x -> isFlowing (mkCoord (x * n)) (flowing st)) [0 ..]

fill :: Clay -> Int -> Coord -> Dir -> State Water Bool
fill clay endy c d = do
  let below        = addCoord 0 1 c
      left         = addCoord (-1) 0 c
      right        = addCoord 1 0 c
      notClayBelow = notClay below clay
      clayLeft     = isClay left clay
      clayRight    = isClay right clay

  modify' (\w -> w { flowing = addFlowing c (flowing w) })

  notFlowingBelow <- gets $ notFlowing below . flowing
  if notClayBelow && notFlowingBelow && cy below <= endy
    then fill clay endy below DirDown
    else pure False

  notSettledBelow <- gets $ notSettled below . settled
  if notClayBelow && notSettledBelow
    then pure False
    else do
      flowingLeft <- gets $ isFlowing left . flowing
      leftFilled  <- if
        | clayLeft    -> state (True, )
        | flowingLeft -> state (False, )
        | otherwise   -> fill clay endy left DirLeft

      flowingRight <- gets $ isFlowing right . flowing
      rightFilled  <- if
        | clayRight    -> state (True, )
        | flowingRight -> state (False, )
        | otherwise    -> fill clay endy right DirRight

      if d == DirDown && leftFilled && rightFilled
        then modify' (`settleRow` c)
        else pure ()

      pure $ (d == DirLeft && leftFilled) || (d == DirRight && rightFilled)

main :: IO ()
main = do
  clay <- parseClay <$> readFile "input/17.txt"
  let Water f s  = flip execState water $ fill clay endy startc DirDown
      water      = Water mempty mempty
      startc     = Coord 500 0
      endy       = maxy $ bbox clay
      starty     = miny $ bbox clay
      flowingSet = unFlowing f
      settledSet = unSettled s
      inRange    = (>= starty) . cy
  print . S.size $ S.filter inRange flowingSet
  print . S.size $ S.filter inRange settledSet
