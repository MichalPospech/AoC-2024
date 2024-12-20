{-# HLINT ignore "Use <$>" #-}
module Day14 (parser, task1, task2) where

import Text.Parsec (sepBy, space, string)
import Text.Parsec.Char (newline)
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Data.Maybe (mapMaybe)
import Data.Text (pack)

type Vector = (Int, Int)

data Robot = Robot Vector Vector

data Bathroom = Bathroom (Int, Int) [Robot]

data Quadrant = NW | NE | SW | SE deriving Eq

parser :: Parser Bathroom
parser = do
  width <- int
  _ <- space
  height <- int
  _ <- newline
  robots <- sepBy robotParser newline
  return $ Bathroom (width, height) robots

robotParser :: Parser Robot
robotParser = do
  _ <- string "p="
  c <- tupleParser
  _ <- string " v="
  v <- tupleParser
  return $ Robot c v

tupleParser :: Parser (Int, Int)
tupleParser = do
  x <- int
  _ <- string ","
  y <- int
  return (x, y)

infix 6 >+<

(>+<) :: Vector -> Vector -> Vector
(x1, y1) >+< (x2, y2) = (x1 + x2, y1 + y2)

infix 7 >*<

(>*<) :: Int -> Vector -> Vector
a >*< (x, y) = (a * x, a * y)


calculatePositionsAfterNSecs :: (Int, Int) -> Int -> [Robot] -> [Vector]
calculatePositionsAfterNSecs b n = map (calculatePositionAfterNSecs b n) 


calculatePositionAfterNSecs :: (Int, Int) -> Int -> Robot -> Vector
calculatePositionAfterNSecs (w, h) s (Robot c v) = (xr, yr)
  where
    (x, y) = c >+< (s >*< v)
    xr = mod x w
    yr = mod y h

positionToQuadrant :: (Int, Int) -> Vector -> Maybe Quadrant
positionToQuadrant (w, h) (x, y) = r
  where
    wBorder = div (w - 1) 2
    hBorder = div (h - 1)  2
    r = case (compare x wBorder, compare y hBorder) of
        (GT, GT) -> Just SE
        (GT, LT) -> Just NE
        (LT, GT) -> Just SW
        (LT, LT) -> Just NW
        _ ->  Nothing

safetyScore :: (Int, Int) -> [Vector] -> Int
safetyScore b positions = score where
    quadrants = mapMaybe (positionToQuadrant b) positions
    score = product $ map (`countOccurences` quadrants) [NW, NE, SW, SE]


countOccurences :: Eq a => a -> [a] -> Int
countOccurences e = length . filter (==e)





task2 = undefined
task1 (Bathroom (w,h) rs)= pack . show . safetyScore (w,h) $ calculatePositionsAfterNSecs (w,h) 100 rs