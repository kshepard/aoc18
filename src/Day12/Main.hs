-- | Day 12

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Control.Lens                   ( element
                                                , (^?)
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( elemIndex )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )

data State =
  State { minIndex :: !Int
        , maxIndex :: !Int
        , getPlant :: Int -> Plant
        }

newtype Plant = Plant { unPlant :: Bool } deriving (Show, Enum, Eq, Ord)
newtype Rules = Rules { matches :: [Plant] -> Plant }

mkState :: String -> State
mkState input = State {..}
 where
  minIndex = fromJust . elemIndex '#' $ input
  maxIndex = length input - (fromJust . elemIndex '#' $ reverse input) - 1
  getPlant n = case input ^? element n of
    Nothing -> Plant False
    Just c  -> Plant $ c == '#'

mkRules :: [String] -> Rules
mkRules ruleStrs = Rules {..}
 where
  matches plants = fromMaybe (Plant False) (M.lookup plants ruleMap)
  mkPlant c = Plant $ c == '#'
  ruleTup s = (mkPlant <$> take 5 s, head $ mkPlant <$> drop 9 s)
  ruleMap = M.fromList $ ruleTup <$> ruleStrs

nextState :: Rules -> State -> State
nextState Rules {..} State {..} = foldl' step emptyState bounds
 where
  emptyState = State maxBound minBound $ const $ Plant False
  bounds     = [minIndex - 2 .. maxIndex + 2]
  step (State accMinI accMaxI accGp) n =
    let plant   = matches $ getPlant <$> [-2 + n .. 2 + n]
        isPlant = plant == Plant True
        newMinI = if isPlant && n < accMinI then n else accMinI
        newMaxI = if isPlant && n > accMaxI then n else accMaxI
        newGp i = if i == n then plant else accGp i
    in  State newMinI newMaxI newGp

plantScore :: State -> Int -> Int
plantScore State {..} n = if getPlant n == Plant True then n else 0

sumPlants :: State -> Int
sumPlants s@State {..} = sum $ plantScore s <$> [minIndex .. maxIndex]

solve :: Rules -> State -> Int -> Int
solve rules initialState gen = sumPlants state
  where state = foldl' (flip . const $ nextState rules) initialState [1 .. gen]

main :: IO ()
main = do
  (i1 : _ : i2) <- lines <$> readFile "input/12.txt"
  let s0    = mkState $ drop 15 i1
      rules = mkRules i2
      part1 = solve rules s0 20
      s100  = solve rules s0 100
      s101  = solve rules s0 101
      part2 = s100 + (50000000000 - 100) * (s101 - s100)
  print part1
  print part2
