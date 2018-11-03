-- | Day 13

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/13.txt"
  print $ init raw
