-- | Day 23

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/23.txt"
  print $ init raw
