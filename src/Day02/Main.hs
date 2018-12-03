-- | Day 02

{-# LANGUAGE TupleSections #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import           Data.List                      ( tails )
import qualified Data.Map                      as M
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( eol
                                                , letterChar
                                                )

type Parser = Parsec Void String

newtype BoxId = BoxId String deriving (Show, Eq, Ord)

boxIdParser :: Parser BoxId
boxIdParser = BoxId <$> many letterChar

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> pure x

frequencyMap :: Ord a => [a] -> M.Map a Int
frequencyMap = M.fromListWith (+) . fmap (, 1)

appearancesTwosThrees :: Ord a => [a] -> (Bool, Bool)
appearancesTwosThrees = foldl' f (False, False) . frequencyMap
  where f (twos, threes) x = (twos || x == 2, threes || x == 3)

appearanceCounts :: [BoxId] -> (Int, Int)
appearanceCounts = foldl' f (0, 0)
 where
  f (twos, threes) (BoxId x) =
    let (apTwo, apThree) = appearancesTwosThrees x
    in  (twos + fromEnum apTwo, threes + fromEnum apThree)

oneOff :: Eq a => [a] -> [a] -> Bool
oneOff xs ys = length (filter (uncurry (/=)) (zip xs ys)) == 1

matchingLetters :: (BoxId, BoxId) -> String
matchingLetters (BoxId x, BoxId y) = fst <$> filter (uncurry (==)) (zip x y)

correctBoxIds :: [BoxId] -> (BoxId, BoxId)
correctBoxIds boxIds = head $ filter
  (\(BoxId x, BoxId y) -> oneOff x y)
  [ (x, y) | (x : xs) <- tails boxIds, y <- xs ]

part1 :: [BoxId] -> Int
part1 boxIds = twos * threes where (twos, threes) = appearanceCounts boxIds

part2 :: [BoxId] -> String
part2 = matchingLetters . correctBoxIds

main :: IO ()
main = do
  boxIds <- parseLines boxIdParser "input/02.txt"
  print $ part1 boxIds
  putStrLn $ part2 boxIds
