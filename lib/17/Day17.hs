{-# LANGUAGE OverloadedStrings #-}

module Day17 (task1, task2, parser) where

import Control.Monad (when)
import Control.Monad.RWS (MonadReader (ask), MonadState, MonadWriter, RWS, execRWS, gets, tell)
import Control.Monad.RWS.Class (modify)
import Data.Bits (xor)
import Text.Parsec ( char, newline, string, sepBy )
import Text.Parsec.Combinator (choice)
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Data.Text (pack, intercalate, Text)

data Program = Program Memory [Instruction]

task1 :: Program -> Text
task1 p  = intercalate "," . map (pack . show ) $ out where
  (_, out) = runEval p

task2 = undefined

parser = do
  m <- memoryParser
  newline
  Program m <$> instructionsParser

data Memory = Memory
  { ip :: Int,
    a :: Int,
    b :: Int,
    c :: Int
  }

type LiteralOperand = Int

type ComboOperand = Int

modifyA :: (Int -> Int) -> Memory -> Memory
modifyA f m = m {a = f aVal}
  where
    aVal = a m

modifyB :: (Int -> Int) -> Memory -> Memory
modifyB f m = m {b = f bVal}
  where
    bVal = b m

modifyC :: (Int -> Int) -> Memory -> Memory
modifyC f m = m {c = f cVal}
  where
    cVal = c m

modifyIp :: (Int -> Int) -> Memory -> Memory
modifyIp f m = m {ip = f ipVal}
  where
    ipVal = ip m

data Instruction = Adv ComboOperand | Bxl LiteralOperand | Bst ComboOperand | Jnz LiteralOperand | Bxc Int | Out ComboOperand | Cdv ComboOperand | Bdv ComboOperand

eval :: (MonadState Memory m, MonadWriter [Int] m) => Instruction -> m ()
eval (Jnz op) = do
  aReg <- gets a
  let modification :: Int -> Int = if aReg == 0 then (+ 1) else const op
  modify (modifyIp modification)
  return ()
eval (Adv op) = evalNormalInstruction $ evalDiv modifyA op
eval (Bdv op) = evalNormalInstruction $ evalDiv modifyB op
eval (Cdv op) = evalNormalInstruction $ evalDiv modifyC op
eval (Bxl lit) = evalNormalInstruction $ do
  bVal <- gets b
  let res = bVal `xor` lit
  modify (modifyB (const res))
eval (Bst op) = evalNormalInstruction $ do
  opVal <- readComboOp op
  let res = opVal `mod` 8
  modify (modifyB (const res))
eval (Bxc _) = evalNormalInstruction $ do
  bVal <- gets b
  cVal <- gets c
  let res = bVal `xor` cVal
  modify (modifyB (const res))
eval (Out op) = evalNormalInstruction $ do
  val <- readComboOp op
  let modVal = val `mod` 8
  tell [modVal]

evalNormalInstruction :: (MonadState Memory m) => m a -> m ()
evalNormalInstruction f = do
  f
  modify (modifyIp (+ 1))
  return ()

readComboOp :: (MonadState Memory m) => Int -> m Int
readComboOp op = case op of
  4 -> gets a
  5 -> gets b
  6 -> gets c
  7 -> undefined
  x -> return x

evalDiv :: (MonadState Memory m) => ((b -> Int) -> Memory -> Memory) -> Int -> m ()
evalDiv modificator op = do
  num <- gets a
  den' <- readComboOp op
  let den = 2 ^ den'
  let res = num `div` den
  modify (modificator (const res))

runEval :: Program -> (Memory, [Int])
runEval (Program m p) = execRWS evalProgram p m

evalProgram :: RWS [Instruction] [Int] Memory ()
evalProgram = do
  p <- ask
  while (gets ((< length p) . ip)) $ do
    ipVal <- gets ip
    let ins = p !! ipVal
    eval ins

while :: (Monad m) => m Bool -> m a -> m ()
while cond act = do
  _ <- act
  b <- cond
  when b $ while cond act

memoryParser :: Parser Memory
memoryParser = do
  string "Register A: "
  a <- int
  newline
  string "Register B: "
  b <- int
  newline
  string "Register C: "
  c <- int
  newline
  return $
    Memory
      { a = a,
        b = b,
        c = c,
        ip = 0
      }


instructionsParser =  do
  string "Program: "
  instructionParser `sepBy` char ','
instructionParser = choice [parseAdv, parseBdv, parseBxc, parseBxl, parseJnz, parseBst, parsecOut, parseCdv]

literalParser :: Parser LiteralOperand
literalParser = int

comboOpParser :: Parser ComboOperand
comboOpParser = int

parseAdv = parseCombo' Adv '0'

parseBxl = parseLit' Bxl '1'

parseBst = parseCombo' Bst '2'

parseJnz = parseLit' Jnz '3'

parseBxc = do
  char '4'
  char ','
  Bxc <$> int

parsecOut = parseCombo' Out '5'

parseBdv = parseCombo' Bdv '6'

parseCdv = parseCombo' Cdv '7'

parseOp' t c opParser = do
  char c
  char ','
  t <$> opParser

parseCombo' t c = parseOp' t c comboOpParser

parseLit' t c = parseOp' t c literalParser

decompileInstr :: Instruction -> [Int]
decompileInstr (Adv op) = [0,op]
decompileInstr (Bxl op) = [1,op]
decompileInstr (Bst op) = [2,op]
decompileInstr (Jnz op) = [3,op]
decompileInstr (Bxc op) = [4,op]
decompileInstr (Out op) = [5,op]
decompileInstr (Bdv op) = [6,op]
decompileInstr (Cdv op) = [7,op]