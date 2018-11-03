-- | Day 04

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/04.txt"
  print $ init raw
