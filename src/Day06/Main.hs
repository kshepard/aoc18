-- | Day 06

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/06.txt"
  print $ init raw
