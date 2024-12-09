{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Day01
import Data.Text
import Data.Attoparsec.Text (parse, Parser, eitherResult)

main :: IO ()
main = putStr "abc"

solve :: Parser a -> (a -> Text) -> Text -> Text
solve parser taskSolver input = let result = eitherResult $ parse parser input in
    case result of 
        Left e -> pack e
        Right r -> taskSolver r