-- | Day 14

{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Data.Char                      ( digitToInt )
import           Data.Foldable                  ( toList, foldl' )
import           Data.List                      ( isInfixOf
                                                , tails
                                                , isPrefixOf
                                                , findIndex
                                                )
import           Data.Maybe                     ( fromJust )
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )

data State =
  State { ind1    :: !Int
        , ind2    :: !Int
        , recipes :: !(Seq Char)
        } deriving (Show)

initialState :: State
initialState = State 0 1 $ Seq.fromList "37"

step :: State -> State
step State {..} = State ind1' ind2' recipes'
 where
  ind1'    = advance ind1 recipe1
  ind2'    = advance ind2 recipe2
  recipes' = foldl' (Seq.|>) recipes $ show (recipe1 + recipe2)
  recipe1  = digitToInt $ recipes `Seq.index` ind1
  recipe2  = digitToInt $ recipes `Seq.index` ind2
  advance i r = (i + r + 1) `rem` length recipes'

part1 :: Int -> State -> String
part1 numRecipes =
  take 10
    . drop numRecipes
    . toList
    . recipes
    . head
    . dropWhile (\s -> length (recipes s) < numRecipes + 10)
    . iterate step

part2 :: String -> State -> Int
part2 recipeSearch =
  fromJust
    . findIndex (isPrefixOf recipeSearch)
    . tails
    . toList
    . recipes
    . head
    . dropWhile (not . isInfixOf recipeSearch . toList . recipes)
    . iterate step

main :: IO ()
main = do
  raw <- init <$> readFile "input/14.txt"
  let input = read raw :: Int
  putStrLn $ part1 input initialState
  print $ part2 (show input) initialState
