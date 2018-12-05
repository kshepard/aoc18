-- | Day 04

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import           Data.List                      ( sort
                                                , maximumBy
                                                )
import qualified Data.Map                      as M
import           Data.Ord                       ( comparing )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                , (<|>)
                                                )
import           Text.Megaparsec.Char           ( eol
                                                , char
                                                , string
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Parser = Parsec Void String

newtype Guard = Guard Int deriving (Show, Eq, Ord)

newtype Minute = Minute Int deriving (Show, Eq, Enum, Num, Ord)

data Instr = Wake | Sleep | BeginShift Guard deriving (Show, Eq)

data Nap =
  Nap { nGuard :: !Guard
      , nMinute :: !Minute
      } deriving (Show, Eq, Ord)

data Record =
  Record { rSortKey :: !String
         , rMinute :: !Minute
         , rInstr :: !Instr
        } deriving (Show, Eq)

instance Ord Record where
  (Record s1 _ _) `compare` (Record s2 _ _) = s1 `compare` s2

wakeParser :: Parser Instr
wakeParser = const Wake <$> string "wakes up"

sleepParser :: Parser Instr
sleepParser = const Sleep <$> string "falls asleep"

beginShiftParser :: Parser Instr
beginShiftParser = do
  string "Guard #"
  guardId <- decimal
  string " begins shift"
  pure $ BeginShift (Guard guardId)

instrParser :: Parser Instr
instrParser = wakeParser <|> sleepParser <|> beginShiftParser

recordParser :: Parser Record
recordParser = do
  char '['
  y <- decimal
  char '-'
  m <- decimal
  char '-'
  d <- decimal
  char ' '
  h <- decimal
  char ':'
  m' <- decimal
  string "] "
  i <- instrParser
  pure $ Record
    { rSortKey = [y, m, d, h, m'] >>= pad
    , rMinute  = Minute m'
    , rInstr   = i
    }
  where pad n = if n < 10 then "0" <> show n else show n

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> return x

frequencyMap :: Ord a => [a] -> M.Map a Int
frequencyMap = M.fromListWith (+) . fmap (, 1)

updateNaps :: Guard -> Minute -> Minute -> [Nap] -> [Nap]
updateNaps guard startNap wakeUp =
  mappend (Nap guard <$> [startNap .. wakeUp - 1])

mkNaps
  :: (Maybe Guard, Maybe Minute, [Nap])
  -> Record
  -> (Maybe Guard, Maybe Minute, [Nap])
mkNaps (mayGuard, mayStartNap, naps) Record {..} = case rInstr of
  Wake             -> (mayGuard, Nothing, newNaps)
  Sleep            -> (mayGuard, Just rMinute, naps)
  BeginShift guard -> (Just guard, Nothing, naps)
 where
  newNaps = case (mayGuard, mayStartNap) of
    (Just guard, Just startNap) -> updateNaps guard startNap rMinute naps
    _                           -> error "Guard/startNap should exist here"

part1 :: [Nap] -> Int
part1 naps = sleepiest * maxMinute
 where
  sg@(Guard sleepiest) = fst . maximumBy (comparing snd) $ sleepCounts
  Minute maxMinute = nMinute . fst . maximumBy (comparing snd) $ minuteCounts
  sleepCounts = M.toList $ frequencyMap $ nGuard <$> naps
  minuteCounts =
    M.toList $ frequencyMap $ filter (\Nap {..} -> nGuard == sg) naps

part2 :: [Nap] -> Int
part2 naps = guardId * minute
 where
  (Nap (Guard guardId) (Minute minute), _) =
    maximumBy (comparing snd) $ M.toList $ frequencyMap naps

main :: IO ()
main = do
  records <- sort <$> parseLines recordParser "input/04.txt"
  let (_, _, naps) = foldl' mkNaps (Nothing, Nothing, mempty) records
  print $ part1 naps
  print $ part2 naps
