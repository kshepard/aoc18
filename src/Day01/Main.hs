-- | Day 01

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

main :: IO ()
main = do
  freqChanges <- parseLines freqChangeParser "input/01.txt"
  print $ sum freqChanges
  let (_, part2, _) =
        head
          . dropWhile (\(_, _, found) -> not found)
          . scanl
              (\(accFreqSet, accFreq, _) currFreq ->
                let newFreq = accFreq + currFreq
                in  ( S.insert newFreq accFreqSet
                    , newFreq
                    , S.member newFreq accFreqSet
                    )
              )
              (S.fromList [FreqChange 0], FreqChange 0, False)
          $ cycle freqChanges
  print part2
