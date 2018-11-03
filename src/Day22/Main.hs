-- | Day 22

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/22.txt"
  print $ init raw
