-- | Day 08

module Main
  ( main
  )
where

import           Control.Lens                   ( element
                                                , (^?)
                                                )
import           Data.Tree                      ( Tree(Node)
                                                , flatten
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , count
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( char )
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Parser = Parsec Void String


treeParser :: Parser (Tree [Int])
treeParser = do
  numChildren <- decimal
  char ' '
  numMeta <- decimal
  char ' '
  children <- count numChildren treeParser
  meta     <- count numMeta (decimal <* many (char ' '))
  pure $ Node meta children

parseInput :: String -> Tree [Int]
parseInput input = case parse (treeParser <* eof) mempty input of
  Left  err -> error $ parseErrorPretty err
  Right x   -> x

nodeValue :: Tree [Int] -> Int
nodeValue (Node []   _       ) = 0
nodeValue (Node meta []      ) = sum meta
nodeValue (Node meta children) = sum $ nodeValue <$> indexedChildren
  where indexedChildren = mapMaybe (\i -> children ^? element (i - 1)) meta

part1 :: Tree [Int] -> Int
part1 = sum . concat . flatten

part2 :: Tree [Int] -> Int
part2 = nodeValue

main :: IO ()
main = do
  tree <- parseInput . init <$> readFile "input/08.txt"
  print $ part1 tree
  print $ part2 tree
