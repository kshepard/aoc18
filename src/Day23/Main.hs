-- | Day 23

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
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

data NanoBot =
  NanoBot { pos    :: !Pos
          , radius :: !Radius
           } deriving (Show)

data Pos =
  Pos { px :: !Int
      , py :: !Int
      , pz :: !Int
      } deriving (Show)

newtype Radius =
  Radius { unRadius :: Int } deriving (Show)

nanoBotParser :: Parser NanoBot
nanoBotParser = do
  x <- string "pos=<" *> signed mempty decimal
  y <- char ',' *> signed mempty decimal
  z <- char ',' *> signed mempty decimal
  r <- string ">, r=" *> decimal
  pure $ NanoBot (Pos x y z) (Radius r)

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> pure x

strongest :: [NanoBot] -> NanoBot
strongest bots = foldl' f (head bots) (tail bots)
 where
  f acc@(NanoBot _ (Radius ra)) b@(NanoBot _ (Radius rb)) =
    if rb > ra then b else acc

manhattan :: Pos -> Pos -> Int
manhattan (Pos x1 y1 z1) (Pos x2 y2 z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

inRange :: NanoBot -> NanoBot -> Bool
inRange (NanoBot p1 (Radius r1)) (NanoBot p2 _) = manhattan p1 p2 <= r1

part1 :: [NanoBot] -> Int
part1 bots = length $ filter (inRange s) bots where s = strongest bots

main :: IO ()
main = do
  nanoBots <- parseLines nanoBotParser "input/23.txt"
  print $ part1 nanoBots
