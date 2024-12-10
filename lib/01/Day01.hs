module Day01 (task1, task2, parser) where

import Data.List (sort)
import Data.MultiSet (fromAscList, occur)
import Data.Text (Text, pack)
import Text.Parsec (endOfLine, skipMany1, space)
import Text.Parsec.Combinator (sepBy1)
import Text.Parsec.Number
import Text.Parsec.Text
  ( Parser  )

parser :: Parser LocationList
parser = do
  uncurry LocationList . unzip <$> dataParser

data LocationList = LocationList [Int] [Int] deriving (Show)

calculateDistance :: LocationList -> Int
calculateDistance (LocationList l1 l2) = sum $ zipWith (\x y -> abs (x - y)) (sort l1) (sort l2)

calculateSimilarity :: LocationList -> Int
calculateSimilarity (LocationList l1 l2) = let occurences = fromAscList $ sort l2 in foldr (\x a -> a + x * occur x occurences) 0 l1

dataParser :: Parser [(Int, Int)]
dataParser = sepBy1 lineParser endOfLine

lineParser :: Parser (Int, Int)
lineParser = do
  left <- int
  skipMany1 space
  right <- int
  return (left, right)

task1 :: LocationList -> Text
task1 input = pack $ show $ calculateDistance input

task2 :: LocationList -> Text
task2 input = pack $ show $ calculateSimilarity input
