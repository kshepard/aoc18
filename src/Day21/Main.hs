-- | Day 21

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/21.txt"
  print $ init raw
