-- | Day 24

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/24.txt"
  print $ init raw
