-- | Day 25

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/25.txt"
  print $ init raw
