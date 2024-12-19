module Day11 (parser, task1, task2) where

import Data.Text (Text, pack)
import Text.Parsec (sepBy, space)
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Data.Function.Memoize (memoize2)

type Stones = [Integer]

parser :: Parser Stones
parser = int `sepBy` space

task1 :: Stones -> Text
task1 = pack . show . sum . map (`getNumStones` 25)

task2 :: Stones -> Text
task2 =  pack . show . sum . map (`getNumStones` 75)


getNumStones :: Integer -> Integer -> Integer
getNumStones _ 0 = 1
getNumStones stoneNum blinksLeft
  | stoneNum == 0 = itself 1 remainingBlinks
  | baseTen `mod` 2 == 1 = getNumStones upperPart remainingBlinks + itself lowerPart remainingBlinks
  | otherwise = itself (stoneNum * 2024) remainingBlinks
  where
    remainingBlinks = blinksLeft - 1
    baseTen = floor (logBase (10 :: Double) (fromInteger stoneNum))
    multipleTen = 10 ^ (baseTen `div` (2 :: Integer) + 1)
    lowerPart = stoneNum `mod` multipleTen
    upperPart = stoneNum `div` multipleTen
    itself = memoize2 getNumStones
