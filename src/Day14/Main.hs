-- | Day 14

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/14.txt"
  print $ init raw
