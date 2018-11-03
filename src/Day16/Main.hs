-- | Day 16

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/16.txt"
  print $ init raw
