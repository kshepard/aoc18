-- | Day 08

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/08.txt"
  print $ init raw
