-- | Day 19

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/19.txt"
  print $ init raw
