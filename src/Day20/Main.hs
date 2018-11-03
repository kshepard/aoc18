-- | Day 20

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/20.txt"
  print $ init raw
