-- | Day 02

module Main
  ( main
  )
where

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

part1 :: [BoxId] -> Int
part1 boxIds = numMatch 2 * numMatch 3
 where
  freq (BoxId input) = M.toList $ M.fromListWith (+) [ (c, 1) | c <- input ]
  freqs = freq <$> boxIds
  mkCounts num = not . null <$> filter (\(_, c) -> c == num)
  numMatch num = length . filter (== True) $ mkCounts num <$> freqs

part2 :: [BoxId] -> String
part2 boxIds = matchingLetters correctBoxIds
 where
  matchingLetters (BoxId x, BoxId y) = fst <$> filter (uncurry (==)) (zip x y)
  oneOff x y = length (filter (uncurry (/=)) (zip x y)) == 1
  correctBoxIds = head $ filter
    (\(BoxId x, BoxId y) -> oneOff x y)
    [ (x, y) | (x : rest) <- tails boxIds, y <- rest ]

main :: IO ()
main = do
  boxIds <- parseLines boxIdParser "input/02.txt"
  print $ part1 boxIds
  print $ part2 boxIds
