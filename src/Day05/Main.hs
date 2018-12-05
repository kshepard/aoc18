-- | Day 05

module Main
  ( main
  )
where

import           Data.Char                      ( toLower )
import           Data.List                      ( nub )

data Unit = Unit Char Bool deriving (Show, Eq)

toUnit :: Char -> Unit
toUnit c = Unit cl (c /= cl) where cl = toLower c

reaction :: Unit -> Unit -> Bool
reaction (Unit c1 b1) (Unit c2 b2) = c1 == c2 && b1 /= b2

runReactions :: [Unit] -> [Unit]
runReactions = foldr step mempty
 where
  step u1 acc@(u2 : us) = if reaction u1 u2 then us else u1 : acc
  step u1 []            = [u1]

part1 :: [Unit] -> Int
part1 = length . runReactions

part2 :: [Unit] -> Int
part2 units = minimum $ fmap (length . runReactions) polymers
 where
  uniqueChars = nub $ (\(Unit c _) -> c) <$> units
  filterOut c = filter (\(Unit c' _) -> c /= c')
  polymers = (`filterOut` units) <$> uniqueChars

main :: IO ()
main = do
  input <- init <$> readFile "input/05.txt"
  let units = toUnit <$> input
  print $ part1 units
  print $ part2 units
