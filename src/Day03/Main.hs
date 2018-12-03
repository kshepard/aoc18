-- | Day 03

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Data.Foldable                  ( foldl' )
import qualified Data.Set                      as S
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( eol
                                                , char
                                                , string
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Parser = Parsec Void String

newtype ClaimId = ClaimId Int deriving (Show, Eq, Ord, Num)

data Point =
  Point { px :: !Int
        , py :: !Int
        } deriving (Show, Eq, Ord)

data Claim =
  Claim { cid :: !ClaimId
        , point :: !Point
        , width :: !Int
        , height :: !Int
        } deriving (Show)

newtype Fabric = Fabric (Point -> S.Set ClaimId)

emptyFabric :: Fabric
emptyFabric = Fabric $ const S.empty

claimParser :: Parser Claim
claimParser = do
  char '#'
  cid <- decimal
  string " @ "
  x <- decimal
  char ','
  y <- decimal
  string ": "
  w <- decimal
  char 'x'
  h <- decimal
  pure $ Claim
    { cid    = ClaimId cid
    , point  = Point {px = fromIntegral x, py = fromIntegral y}
    , width  = fromIntegral w
    , height = fromIntegral h
    }

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> return x

pointsAffected :: Claim -> S.Set Point
pointsAffected Claim {..} = S.fromList
  [ Point {px = x, py = y}
  | x <- [cx .. cx + width - 1]
  , y <- [cy .. cy + height - 1]
  ]
 where
  cx = px point
  cy = py point

fabricPoints :: [Claim] -> [Point]
fabricPoints = S.toList . foldl' (\s c -> S.union s (pointsAffected c)) S.empty

updateFabric :: Fabric -> Claim -> Fabric
updateFabric (Fabric f) c@Claim {..} = Fabric
  $ \p -> if S.member p (pointsAffected c) then S.insert cid (f p) else f p

part1 :: [S.Set ClaimId] -> Int
part1 = length . filter (\cs -> length cs > 1)

part2 :: [ClaimId] -> [S.Set ClaimId] -> ClaimId
part2 claimIds = head . S.toList . foldl' f (S.fromList claimIds)
  where f acc set = if length set > 1 then S.difference acc set else acc

main :: IO ()
main = do
  claims <- parseLines claimParser "input/03.txt"
  let (Fabric f) = foldl' updateFabric emptyFabric claims
      points     = fabricPoints claims
      claimIds   = cid <$> claims
      claimSets  = f <$> points
  print $ part1 claimSets
  print $ part2 claimIds claimSets
