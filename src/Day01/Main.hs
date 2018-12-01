-- | Day 01

module Main
  ( main
  )
where

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

newtype FreqChange = FreqChange Int deriving (Eq, Num, Ord, Show)

freqChangeParser :: Parser FreqChange
freqChangeParser = FreqChange <$> signed mempty decimal

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> pure x

part2 :: S.Set FreqChange -> [FreqChange] -> FreqChange -> FreqChange
part2 _ [] _ = error "Empty list"
part2 freqSet (curr : rest) acc | S.member newFreq freqSet = newFreq
                                | otherwise = part2 newFreqSet rest newFreq
 where
  newFreq    = acc + curr
  newFreqSet = S.insert newFreq freqSet

main :: IO ()
main = do
  freqChanges <- parseLines freqChangeParser "input/01.txt"
  print $ sum freqChanges
  print $ part2 (S.fromList [FreqChange 0]) (cycle freqChanges) (FreqChange 0)
