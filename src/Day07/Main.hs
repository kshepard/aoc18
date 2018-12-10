-- | Day 07

module Main
  ( main
  )
where

import qualified Data.List                     as L
import qualified Data.Map                      as M
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

chooseStep :: M.Map Step (S.Set Step) -> Step
chooseStep = minimum . availableSteps

part1 :: M.Map Step (S.Set Step) -> [Step]
part1 = L.unfoldr next
 where
  next steps = if M.null steps
    then Nothing
    else
      let chosen   = chooseStep steps
          newSteps = M.map (S.delete chosen) $ M.delete chosen steps
      in  Just (chosen, newSteps)

main :: IO ()
main = do
  deps <- M.fromListWith S.union <$> parseLines stepParser "input/07.txt"
  putStrLn $ unStep <$> (part1 deps <> [lastStep deps])
