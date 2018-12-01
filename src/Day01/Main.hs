-- | Day 01

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import qualified Data.Set                      as S
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( eol )
import           Text.Megaparsec.Char.Lexer     ( signed
                                                , decimal
                                                )

type Parser = Parsec Void String

newtype FreqChange = FreqChange Int deriving (Show, Eq, Ord)

freqChangeParser :: Parser FreqChange
freqChangeParser = FreqChange <$> signed mempty decimal

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> pure x

part1 :: [FreqChange] -> FreqChange
part1 = foldl' (\(FreqChange acc) (FreqChange n) -> FreqChange (acc + n))
               (FreqChange 0)

part2 :: [FreqChange] -> FreqChange
part2 freqChanges =
  part2Helper (S.fromList [FreqChange 0]) (cycle freqChanges) (FreqChange 0)

part2Helper :: S.Set FreqChange -> [FreqChange] -> FreqChange -> FreqChange
part2Helper _ [] _ = error "Empty list"
part2Helper freqSet (FreqChange curr : rest) (FreqChange acc)
  | S.member newFreq freqSet = newFreq
  | otherwise                = part2Helper newFreqSet rest newFreq
 where
  newFreq    = FreqChange (acc + curr)
  newFreqSet = S.insert newFreq freqSet

main :: IO ()
main = do
  freqChanges <- parseLines freqChangeParser "input/01.txt"
  print $ part1 freqChanges
  print $ part2 freqChanges
