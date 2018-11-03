-- | Day 12

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/12.txt"
  print $ init raw
