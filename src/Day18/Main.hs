-- | Day 18

module Main
  ( main
  )
where

main :: IO ()
main = do
  raw <- readFile "input/18.txt"
  print $ init raw
