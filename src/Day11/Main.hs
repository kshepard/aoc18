-- | Day 11

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/11.txt"
  print $ init raw
