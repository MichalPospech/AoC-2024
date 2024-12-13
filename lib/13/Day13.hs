{-# LANGUAGE DeriveFunctor #-}

module Day13 (parser, task1, task2) where

import Data.Ratio (denominator, numerator)
import Data.Text (Text, pack)
import Text.Parsec (eof, newline, string, (<|>))
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)

data Button a = Button a a deriving (Functor, Show)

data Prize a = Prize a a deriving (Functor, Show)

data ClawMachine a = ClawMachine (Button a) (Button a) (Prize a) deriving (Functor, Show)

parser :: Parser [ClawMachine Int]
parser = do
  c <- clawMachineParser `sepBy` newline
  eof
  return c

clawMachineParser :: Parser (ClawMachine Int)
clawMachineParser = do
  a <- buttonParser
  b <- buttonParser
  ClawMachine a b <$> prizeParser

buttonParser :: Parser (Button Int)
buttonParser = do
  _ <- string "Button "
  _ <- string "B: X+" <|> string "A: X+"
  x <- int
  _ <- string ", Y+"
  y <- int
  _ <- newline
  return $ Button x y

prizeParser :: Parser (Prize Int)
prizeParser = do
  _ <- string "Prize: X="
  x <- int
  _ <- string ", Y="
  y <- int
  _ <- newline
  return $ Prize x y

calculateCoins :: ClawMachine Int -> Integer
calculateCoins c = case calculatePresses c of
  Nothing -> 0
  Just (a, b) -> (if denominator a == 1 && denominator b == 1 then numerator (a * 3 + b) else 0)

calculatePresses :: ClawMachine Int -> Maybe (Rational, Rational)
calculatePresses c = do
  let (ClawMachine (Button x1 y1) (Button x2 y2) (Prize x y)) = toRational <$> c
  let b = (y * x1 - y1 * x) / (x1 * y2 - x2 * y1)
  let a = (x - b * x2) / x1
  if 0 /= (x1 * y2 - x2 * y1) then Just (a, b) else Nothing -- doesn't solve the linearly dependent case properly, but still works

task1 :: [ClawMachine Int] -> Text
task1 = pack . show . sum . map calculateCoins

task2 :: [ClawMachine Int] -> Text
task2 = pack . show . sum . map (calculateCoins . movePrizes)

movePrizes :: (Num a) => ClawMachine a -> ClawMachine a
movePrizes (ClawMachine a b p) = ClawMachine a b ((+ 10000000000000) <$> p)
