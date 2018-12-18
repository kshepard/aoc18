-- | Day 16

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main
  ( main
  )
where

import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import           Data.Foldable                  ( foldl' )
import qualified Data.IntMap.Strict            as IM
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( char
                                                , eol
                                                , newline
                                                , space
                                                , string
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Parser = Parsec Void String

data Instruction =
  Instruction { opCode :: !Int
              , opA :: !Int
              , opB :: !Int
              , opC :: !Int
              } deriving (Show)

newtype Registers =
  Registers { regs :: IntMap Int } deriving (Show, Eq)

data Sample =
  Sample { before :: !Registers
         , instruction :: !Instruction
         , after :: !Registers
         } deriving (Show)

data Manual =
  Manual { samples :: ![Sample]
         , instructions :: ![Instruction]
         } deriving (Show)

newtype OpName =
  OpName { name :: String } deriving (Show, Eq, Ord)

registersParser :: Parser Registers
registersParser = do
  r0 <- char '[' *> decimal <* string ", "
  r1 <- decimal <* string ", "
  r2 <- decimal <* string ", "
  r3 <- decimal <* char ']'
  pure . Registers . IM.fromList $ [0 ..] `zip` [r0, r1, r2, r3]

instructionParser :: Parser Instruction
instructionParser = do
  opCode <- decimal <* space
  opA    <- decimal <* space
  opB    <- decimal <* space
  opC    <- decimal <* eol
  pure Instruction {..}

sampleParser :: Parser Sample
sampleParser = do
  before      <- string "Before: " *> registersParser <* eol
  instruction <- instructionParser <* string "After:  "
  after       <- registersParser <* eol <* newline
  pure Sample {..}

manualParser :: Parser Manual
manualParser = do
  samples      <- many sampleParser <* newline <* newline
  instructions <- many instructionParser <* eof
  pure Manual {..}

parseManual :: String -> Manual
parseManual input = case parse manualParser mempty input of
  Left  err -> error $ parseErrorPretty err
  Right x   -> x

opsByName :: Map OpName (Instruction -> Registers -> Registers)
opsByName = M.fromList
  [ ( OpName "addr"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (regs IM.! opA + regs IM.! opB) regs
    )
  , ( OpName "addi"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (regs IM.! opA + opB) regs
    )
  , ( OpName "mulr"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (regs IM.! opA * regs IM.! opB) regs
    )
  , ( OpName "muli"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (regs IM.! opA * opB) regs
    )
  , ( OpName "banr"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (regs IM.! opA .&. regs IM.! opB) regs
    )
  , ( OpName "bani"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (regs IM.! opA .&. opB) regs
    )
  , ( OpName "borr"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (regs IM.! opA .|. regs IM.! opB) regs
    )
  , ( OpName "bori"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (regs IM.! opA .|. opB) regs
    )
  , ( OpName "setr"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (regs IM.! opA) regs
    )
  , ( OpName "seti"
    , \Instruction {..} Registers {..} -> Registers $ IM.insert opC opA regs
    )
  , ( OpName "gtir"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (fromEnum $ opA > regs IM.! opB) regs
    )
  , ( OpName "gtri"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (fromEnum $ regs IM.! opA > opB) regs
    )
  , ( OpName "gtrr"
    , \Instruction {..} Registers {..} -> Registers
      $ IM.insert opC (fromEnum $ regs IM.! opA > regs IM.! opB) regs
    )
  , ( OpName "eqir"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (fromEnum $ opA == regs IM.! opB) regs
    )
  , ( OpName "eqri"
    , \Instruction {..} Registers {..} ->
      Registers $ IM.insert opC (fromEnum $ regs IM.! opA == opB) regs
    )
  , ( OpName "eqrr"
    , \Instruction {..} Registers {..} -> Registers
      $ IM.insert opC (fromEnum $ regs IM.! opA == regs IM.! opB) regs
    )
  ]

opNames :: [OpName]
opNames = M.keys opsByName

potential :: Sample -> OpName -> Bool
potential Sample {..} opName =
  (opsByName M.! opName) instruction before == after

candidates :: Sample -> [OpName]
candidates sample = filter (potential sample) opNames

candidatesByCode :: Manual -> IntMap (Set OpName)
candidatesByCode Manual {..} = foldl' step initial samples
 where
  initial = IM.fromList $ (, S.fromList opNames) <$> [0 .. 15]
  step im sample =
    let cs1  = im IM.! code
        cs2  = S.fromList (candidates sample)
        code = opCode $ instruction sample
        intr = cs1 `S.intersection` cs2
    in  IM.insert code intr im

opNamesByCode :: Manual -> IntMap OpName
opNamesByCode manual = step (candidatesByCode manual)
 where
  singles =
    S.fromList
      . fmap (S.elemAt 0)
      . filter (\s -> length s == 1)
      . fmap snd
      . IM.toList
  removeSingles cbc s = if length s == 1 then s else s S.\\ singles cbc
  done cbc = length (singles cbc) == 16
  step cbc | done cbc  = S.elemAt 0 <$> cbc
           | otherwise = step $ removeSingles cbc <$> cbc

part1 :: Manual -> Int
part1 Manual {..} =
  length . filter (\s -> length (candidates s) >= 3) $ samples

part2 :: Manual -> Int
part2 manual@Manual {..} = registers IM.! 0
 where
  Registers registers = foldl' step initial instructions
  onbc                = opNamesByCode manual
  initial             = Registers . IM.fromList $ [0 ..] `zip` replicate 4 0
  step regs instr =
    let opName = onbc IM.! opCode instr
        op     = opsByName M.! opName
    in  op instr regs

main :: IO ()
main = do
  manual <- parseManual <$> readFile "input/16.txt"
  print $ part1 manual
  print $ part2 manual
