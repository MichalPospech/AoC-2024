module Day11 (parser, task1, task2) where

import Data.Text (Text, pack)
import Text.Parsec (sepBy, space)
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)

type Stones = [Integer]

parser :: Parser Stones
parser = int `sepBy` space

task1 :: Stones -> Text
task1 = pack . show . sum . map (`getNumStones` 25)

task2 :: Stones -> Text
task2 = undefined

getNumStones :: Integer -> Integer -> Integer
getNumStones _ 0 = 1
getNumStones stoneNum blinksLeft
  | stoneNum == 0 = getNumStones 1 remainingBlinks
  | baseTen `mod` 2 == 1 = getNumStones upperPart remainingBlinks + getNumStones lowerPart remainingBlinks
  | otherwise = getNumStones (stoneNum * 2024) remainingBlinks
  where
    remainingBlinks = blinksLeft - 1
    baseTen = floor (logBase 10 (fromInteger stoneNum))
    multipleTen = 10 ^ (baseTen `div` 2 + 1)
    lowerPart = stoneNum `mod` multipleTen
    upperPart = stoneNum `div` multipleTen
