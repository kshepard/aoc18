-- | Day 24

{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import           Data.List                      ( delete
                                                , sortOn
                                                )
import qualified Data.IntMap                   as IM
import           Data.IntMap                    ( IntMap )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( eof
                                                , many
                                                , option
                                                , parse
                                                , parseErrorPretty
                                                , sepBy1
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( char
                                                , eol
                                                , letterChar
                                                , space
                                                , string
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Parser = Parsec Void String

data UnitType =
  ImmuneSystem | Infection deriving (Eq, Ord, Show)

data EffectType =
  Weak | Immune deriving (Eq, Ord, Show)

data AttackType =
  Bludgeoning | Fire | Slashing | Radiation | Cold deriving (Eq, Ord, Show)

data Group =
  Group { unitType   :: !UnitType
        , numUnits   :: !Int
        , hitPoints  :: !Int
        , weaknesses :: ![AttackType]
        , immunities :: ![AttackType]
        , attackAmt  :: !Int
        , attackType :: !AttackType
        , initiative :: !Int
        } deriving (Eq, Ord, Show)

attackTypeParser :: Parser AttackType
attackTypeParser = do
  atStr <- many letterChar
  pure $ case atStr of
    "bludgeoning" -> Bludgeoning
    "fire"        -> Fire
    "slashing"    -> Slashing
    "radiation"   -> Radiation
    "cold"        -> Cold
    _             -> error "Invalid attack type"

effectTypeParser :: Parser EffectType
effectTypeParser = do
  etStr <- many letterChar
  pure $ case etStr of
    "weak"   -> Weak
    "immune" -> Immune
    _        -> error "Invalid effect type"

propsParser :: Parser ([AttackType], [AttackType])
propsParser = option ([], []) $ char '(' *> propsSectionsParser <* string ") "

propsSectionsParser :: Parser ([AttackType], [AttackType])
propsSectionsParser = do
  tups <- sepBy1 propsSectionParser $ string "; "
  let f (w, i) (et, ats) = if et == Weak then (w ++ ats, i) else (w, i ++ ats)
  pure $ foldl' f ([], []) tups

propsSectionParser :: Parser (EffectType, [AttackType])
propsSectionParser = do
  effectType  <- effectTypeParser <* string " to "
  attackTypes <- attackTypeParser `sepBy1` string ", "
  pure (effectType, attackTypes)

groupParser :: UnitType -> Parser Group
groupParser unitType = do
  numUnits                 <- decimal <* string " units each with "
  hitPoints                <- decimal <* string " hit points "
  (weaknesses, immunities) <- propsParser <* string "with an attack that does "
  attackAmt                <- decimal <* space
  attackType               <- attackTypeParser
  initiative               <- string " damage at initiative " *> decimal <* eol
  pure Group {..}

groupsParser :: Parser [Group]
groupsParser = do
  string "Immune System:" <* eol
  immuneSystemGroups <- many (groupParser ImmuneSystem) <* eol
  string "Infection:" <* eol
  infectionGroups <- many (groupParser Infection)
  pure $ immuneSystemGroups ++ infectionGroups

parseInput :: String -> [Group]
parseInput input = case parse (groupsParser <* eof) mempty input of
  Left  err -> error $ parseErrorPretty err
  Right x   -> x

effectivePower :: Group -> Int
effectivePower Group {..} = numUnits * attackAmt

sortTargetingPriority :: [Group] -> [Group]
sortTargetingPriority =
  sortOn $ \g -> (negate $ effectivePower g, negate $ initiative g)

sortBestTarget :: [(Group, Int)] -> [(Group, Int)]
sortBestTarget = sortOn
  $ \(g, da) -> (negate da, negate $ effectivePower g, negate $ initiative g)

damageAmt :: Group -> Group -> Int
damageAmt attacker defender | att `elem` immunities defender = 0
                            | att `elem` weaknesses defender = 2 * ata * units
                            | otherwise                      = ata * units
 where
  att   = attackType attacker
  ata   = attackAmt attacker
  units = numUnits attacker

bestTarget :: Group -> [Group] -> Maybe Group
bestTarget attacker groups | null defenders || amt <= 0 = Nothing
                           | otherwise                  = Just bestTarget'
 where
  defenders          = filter (\g -> unitType g /= unitType attacker) groups
  targetsWithDmg     = (\d -> (d, damageAmt attacker d)) <$> defenders
  (bestTarget', amt) = head $ sortBestTarget targetsWithDmg

attackOrder :: [Group] -> [(Int, Int)]
attackOrder groups = idPairs
 where
  idPairs = (\(a, d) -> (initiative a, initiative d)) <$> sortedAttackPairs
  sortedAttackPairs = sortOn (\(a, _) -> negate (initiative a)) attackPairs
  attackPairs = fst . foldl' f ([], groups) $ sortTargetingPriority groups
  f (acc, targets) g = case best of
    Nothing -> (acc, targets)
    Just t  -> ((g, t) : acc, delete t targets)
    where best = bestTarget g targets

attack :: IntMap Group -> (Int, Int) -> IntMap Group
attack groupById (attackerId, defenderId) = case (attacker, defender) of
  (Just a, Just d) ->
    if numUnits a <= 0 || numUnits d <= 0 then groupById else doAttack a d
  _ -> groupById
 where
  attacker = IM.lookup attackerId groupById
  defender = IM.lookup defenderId groupById
  doAttack a d | remaining > 0 = IM.insert defenderId updated groupById
               | otherwise     = IM.delete defenderId groupById
   where
    damage    = damageAmt a d
    killed    = min (numUnits d) $ damage `quot` hitPoints d
    remaining = numUnits d - killed
    updated   = d { numUnits = remaining }

fight :: [Group] -> [Group]
fight groups = IM.elems . foldl' attack groupById $ attackOrder groups
  where groupById = IM.fromList [ (initiative g, g) | g <- groups ]

simulate :: [Group] -> [Group]
simulate groups | groups == groups' = groups
                | otherwise         = simulate groups'
  where groups' = fight groups

part1 :: [Group] -> Int
part1 = sum . fmap numUnits . simulate

main :: IO ()
main = do
  groups <- parseInput <$> readFile "input/24.txt"
  print $ part1 groups
