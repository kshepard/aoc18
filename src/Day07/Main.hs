-- | Day 07

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main
  ( main
  )
where

import           Data.Char                      ( ord )
import           Data.Foldable                  ( foldl' )
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import qualified Data.Set                      as S
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( eol
                                                , string
                                                , upperChar
                                                )

type Parser = Parsec Void String

newtype Step = Step { unStep :: Char }
  deriving (Show, Ord, Eq)

newtype Seconds = Seconds { unSeconds :: Int }
  deriving (Show, Num, Eq)

data Worker = Worker
  { workingOn :: Step
  , remaining :: Seconds
  } deriving (Show)

stepParser :: Parser (Step, S.Set Step)
stepParser = do
  string "Step "
  step1 <- upperChar
  string " must be finished before step "
  step2 <- upperChar
  string " can begin."
  pure (Step step1, S.singleton $ Step step2)

parseLines :: Parser a -> String -> IO [a]
parseLines parser path = do
  input <- readFile path
  case parse (many (parser <* eol) <* eof) mempty input of
    Left  err -> error $ parseErrorPretty err
    Right x   -> pure x

availableSteps :: M.Map Step (S.Set Step) -> [Step]
availableSteps deps =
  M.keys deps L.\\ concat (S.toList . snd <$> M.toList deps)

lastStep :: M.Map Step (S.Set Step) -> Step
lastStep deps =
  head $ concat (S.toList . snd <$> M.toList deps) L.\\ M.keys deps

chooseStep :: [Step] -> Maybe Step
chooseStep steps | L.null steps = Nothing
                 | otherwise    = Just $ minimum steps

removeStep :: Step -> M.Map Step (S.Set Step) -> M.Map Step (S.Set Step)
removeStep s = M.map (S.delete s) . M.delete s

part1 :: M.Map Step (S.Set Step) -> [Step]
part1 = L.unfoldr next
 where
  next deps = if M.null deps
    then Nothing
    else case chooseStep (availableSteps deps) of
      Nothing -> Nothing
      Just s  -> Just (s, removeStep s deps)

ttl :: Step -> Seconds
ttl (Step s) = Seconds (addlSeconds + 1 + ord s - ord 'A')

part2 :: M.Map Step (S.Set Step) -> Seconds
part2 = go 0 mempty
 where
  go elapsed workers deps
    | M.null deps
    = elapsed
    | length workers < numWorkers
    , isJust nextStep
    = let next = fromJust nextStep
      in  go elapsed (Worker next (ttl next) : workers) deps
    | not (L.null finished)
    = let stepsDone = workingOn <$> finished
          deps'     = foldl' (flip removeStep) deps stepsDone
      in  go elapsed working deps'
    | otherwise
    = let tickDown w = w { remaining = remaining w - 1 }
          workers' = tickDown <$> workers
      in  go (elapsed + Seconds 1) workers' deps
   where
    available = availableSteps deps L.\\ (workingOn <$> workers)
    nextStep  = chooseStep available
    finished  = filter (\w -> remaining w == 0) workers
    working   = filter (\w -> remaining w /= 0) workers

addlSeconds :: Int
addlSeconds = 60

numWorkers :: Int
numWorkers = 5

main :: IO ()
main = do
  deps <- M.fromListWith S.union <$> parseLines stepParser "input/07.txt"
  putStrLn $ unStep <$> (part1 deps <> [lastStep deps])
  print . unSeconds $ part2 deps + ttl (lastStep deps)
