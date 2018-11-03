-- | Day 07

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/07.txt"
  print $ init raw
