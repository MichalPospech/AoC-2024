{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Text
import Data.Text.IO (putStrLn, readFile)
import Day01 qualified
import Day09 qualified
import Day10 qualified
import Day11 qualified
import Day13 qualified
import Options.Applicative
import Text.Parsec (parse)
import Text.Parsec.Text qualified as P (Parser)
import Text.Printf (printf)
import Text.Show (show)
import Prelude ( Either(..), IO, Int, undefined, ($), (=<<), Show )

data Args = Args Int Int Text

argsParser :: Parser Args
argsParser = Args <$> argument auto (metavar "day") <*> argument auto (metavar "task") <*> argument str (metavar "input-name")

opts :: ParserInfo Args
opts = info (argsParser <**> helper) fullDesc

main :: IO ()
main = run =<< execParser opts

run :: Args -> IO ()
run (Args day task file) = do
  let daySolution = daySolvers day
  input <- readFile $ printf "./data/%02d/%s" day file
  let solver = case task of
        1 -> solver1 daySolution
        2 -> solver2 daySolution
        _ -> undefined
  let result = solver input
  putStrLn result

solve :: P.Parser a -> (a -> Text) -> Text -> Text
solve parser taskSolver input = do
  let taskInput = parse parser "infile" input
   in case taskInput of
        Left e -> pack $ show e
        Right p -> taskSolver p

data DaySolver = DaySolver
  { solver1 :: Text -> Text,
    solver2 :: Text -> Text
  }

daySolvers :: Int -> DaySolver
daySolvers 1 = DaySolver (solve Day01.parser Day01.task1) (solve Day01.parser Day01.task2)
daySolvers 9 = DaySolver (solve Day09.parser Day09.task1) (solve Day09.parser Day09.task2)
daySolvers 10 = DaySolver (solve Day10.parser Day10.task1) (solve Day10.parser Day10.task2)
daySolvers 11 = DaySolver (solve Day11.parser Day11.task1) (solve Day11.parser Day11.task2)
daySolvers 13 = DaySolver (solve Day13.parser Day13.task1) (solve Day13.parser Day13.task2)


daySolvers _ = undefined