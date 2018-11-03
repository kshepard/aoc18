-- | Day 03

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/03.txt"
  print $ init raw
