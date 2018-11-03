-- | Day 09

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/09.txt"
  print $ init raw
