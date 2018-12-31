-- | Day 25

{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( char
                                                , eol
                                                )
import           Text.Megaparsec.Char.Lexer     ( signed
                                                , decimal
                                                )

type Parser = Parsec Void String

data Pos =
  Pos { pw :: !Int
      , px :: !Int
      , py :: !Int
      , pz :: !Int
      } deriving (Show, Eq, Ord)

positionParser :: Parser Pos
positionParser = do
  pw <- signed mempty decimal <* char ','
  px <- signed mempty decimal <* char ','
  py <- signed mempty decimal <* char ','
  pz <- signed mempty decimal
  pure $ Pos {..}

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> pure x

manhattan :: Pos -> Pos -> Int
manhattan (Pos w1 x1 y1 z1) (Pos w2 x2 y2 z2) =
  abs (w2 - w1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

connected :: Pos -> Set Pos -> Bool
connected pos = any $ \p -> manhattan p pos <= 3

addPos :: Pos -> [Set Pos] -> [Set Pos]
addPos pos = go (S.singleton pos) []
 where
  go newC others [] = newC : others
  go newC others (c : rest) | connected pos c = go (S.union c newC) others rest
                            | otherwise       = go newC (c : others) rest

main :: IO ()
main = do
  positions <- parseLines positionParser "input/25.txt"
  print . length $ foldl' (flip addPos) [] positions
