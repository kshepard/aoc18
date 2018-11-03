-- | Day 15

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/15.txt"
  print $ init raw
