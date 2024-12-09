module Day01 (task1, task2, parser1, parser2) where
import Data.Text (Text, pack)
import Data.List (sort)
import Data.Attoparsec.Text
    ( sepBy, decimal, skipSpace, endOfLine, Parser )

parser1 :: Parser LocationList
parser1 = do
    uncurry LocationList . unzip <$> dataParser
parser2 :: Parser LocationList
parser2 = parser1


data LocationList = LocationList [Integer] [Integer]



calculateDistance :: LocationList -> Integer
calculateDistance (LocationList l1 l2) = sum $ zipWith  (\ x y -> abs (x-y)) (sort l1) (sort l2)
dataParser :: Parser [(Integer,Integer)]
dataParser = sepBy lineParser endOfLine
lineParser :: Parser (Integer,Integer)
lineParser = do
    left <- decimal
    skipSpace
    right <- decimal
    return (left, right)


task1 :: LocationList -> Text
task1 input = pack $ show $ calculateDistance input
task2 :: () -> Text
task2 = undefined
