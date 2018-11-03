-- | Day 02

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/02.txt"
  print $ init raw
