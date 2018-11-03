-- | Day 05

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/05.txt"
  print $ init raw
