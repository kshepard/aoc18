-- | Day 17

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/17.txt"
  print $ init raw
