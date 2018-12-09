-- | Day 09

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromJust )
import qualified Data.List.PointedList.Circular
                                               as PL
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( string )
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Parser = Parsec Void String

data Rules =
  Rules { lastPlayer :: Player
        , lastMarble :: Marble
        } deriving (Show)

newtype Player = Player
  { unPlayer :: Int }
  deriving (Ord, Eq, Num, Show, Enum, Real, Integral)

newtype Marble = Marble
  { unMarble :: Int }
  deriving (Ord, Eq, Num, Show, Enum, Real, Integral)

newtype Score = Score
  { unScore :: Int }
  deriving (Ord, Eq, Num, Show, Enum, Real, Integral)

rulesParser :: Parser Rules
rulesParser = do
  lastPlayer <- decimal
  string " players; last marble is worth "
  lastMarble <- decimal
  string " points"
  pure Rules {..}

parseInput :: String -> Rules
parseInput input = case parse (rulesParser <* eof) mempty input of
  Left  err -> error $ parseErrorPretty err
  Right x   -> x

playTurn
  :: (M.Map Player Score, PL.PointedList Marble, PL.PointedList Player)
  -> Marble
  -> (M.Map Player Score, PL.PointedList Marble, PL.PointedList Player)
playTurn (scores, circle, players) marble = if marble `mod` 23 == 0
  then
    let moved7     = PL.moveN (-7) circle
        currScore  = Score . unMarble $ PL._focus moved7 + marble
        currPlayer = PL._focus players
        newScores  = M.insertWith (+) currPlayer currScore scores
        newCircle  = fromJust $ PL.deleteRight moved7
    in  (newScores, newCircle, newPlayers)
  else (scores, PL.insertLeft marble . PL.moveN 2 $ circle, newPlayers)
  where newPlayers = PL.moveN 1 players

playGame :: Rules -> Int
playGame Rules {..} = unScore . maximum $ scores
 where
  (scores, _, _) = foldl' playTurn initialState [1 .. lastMarble]
  initialState =
    (M.empty, PL.singleton 0, fromJust $ PL.fromList [1 .. lastPlayer])

main :: IO ()
main = do
  rules <- parseInput . init <$> readFile "input/09.txt"
  print $ playGame rules
  print $ playGame (rules { lastMarble = 100 * lastMarble rules })
