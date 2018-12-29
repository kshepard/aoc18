-- | Day 21

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import           Data.List                      ( elemIndex )
import qualified Data.IntMap.Strict            as IM
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.Map.Strict               as M
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromJust )
import qualified Data.Set                      as S
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( many
                                                , eof
                                                , parse
                                                , parseErrorPretty
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( letterChar
                                                , eol
                                                , space
                                                , string
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )

type Parser = Parsec Void String

data Instruction =
  Instruction { opName :: !OpName
              , opA    :: !Int
              , opB    :: !Int
              , opC    :: !Int
              } deriving (Show)

newtype Registers =
  Registers { regs :: IntMap Int } deriving (Show, Eq)

data Manual =
  Manual { ipRegister :: !Int
         , instructions :: ![Instruction]
         } deriving (Show)

newtype OpName =
  OpName { name :: String } deriving (Show, Eq, Ord)

instructionParser :: Parser Instruction
instructionParser = do
  opName <- OpName <$> many letterChar <* space
  opA    <- decimal <* space
  opB    <- decimal <* space
  opC    <- decimal <* eol
  pure Instruction {..}

ipRegisterParser :: Parser Int
ipRegisterParser = string "#ip " *> decimal <* eol

manualParser :: Parser Manual
manualParser = do
  ipRegister   <- ipRegisterParser
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

emptyRegisters :: Registers
emptyRegisters = Registers . IM.fromList $ [0 ..] `zip` replicate 6 0

runProgram :: Manual -> Int -> Int -> Registers -> [Int]
runProgram Manual {..} sv si = go
 where
  go r@(Registers rs) = if
    | instrIndex >= length instructions -> []
    | instrIndex == si                  -> rs IM.! sv : go newRegs'
    | otherwise                         -> go newRegs'
   where
    instrIndex = rs IM.! ipRegister
    instr      = instructions !! instrIndex
    op         = opsByName M.! opName instr
    newRegs    = op instr r
    newRegs'   = Registers $ IM.update (Just . (1 +)) ipRegister (regs newRegs)

findCycle :: [Int] -> Int
findCycle = go 0 S.empty
 where
  go curr set vals | S.member h set = curr
                   | otherwise      = go h (S.insert h set) t
   where
    h = head vals
    t = tail vals

main :: IO ()
main = do
  manual <- parseManual <$> readFile "input/21.txt"
  let instrs = instructions manual
      sv     = opC $ head instrs
      si     = fromJust (elemIndex (OpName "eqrr") (opName <$> instrs)) + 1
      vals   = runProgram manual sv si emptyRegisters
  print $ head vals
  print $ findCycle vals
