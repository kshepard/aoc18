-- | Day 01

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/01.txt"
  print $ init raw
