-- | Day 10

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/10.txt"
  print $ init raw
