module Day19 (parser, task1, task2) where

import Control.Monad.State (gets, modify, runState)
import Control.Monad.State.Lazy (MonadState)
import Data.Function (fix)
import Data.List (isPrefixOf)
import Data.Map (empty, insert, lookup)
import Data.Map.Strict (Map)
import Data.Text (Text, pack)
import Text.Parsec (newline, oneOf, sepBy, string)
import Text.Parsec.Prim (many)
import Text.Parsec.Text (Parser)

type Towel = String

type Pattern = String

parser :: Parser ([Towel], [Pattern])
parser = do
  towels <- towelsParser
  _ <- many newline
  patterns <- patternParser
  return (towels, patterns)

task1 :: ([Towel], [Pattern]) -> Text
task1 (towels, patterns) = pack $ show res
  where
    (res, _) = runState (countPossible towels patterns) empty

task2 :: ([Towel], [Pattern]) -> Text
task2 (towels, patterns) = pack $ show res
  where
    (res, _) = runState (countPossibleWays towels patterns) empty

towelsParser :: Parser [Towel]
towelsParser = many (oneOf ['w', 'u', 'b', 'r', 'g']) `sepBy` string ", "

patternParser :: Parser [Pattern]
patternParser = many (oneOf ['w', 'u', 'b', 'r', 'g']) `sepBy` newline

countPossible :: (Monad m, MonadState (Map Pattern Int) m) => [Towel] -> [Pattern] -> m Int
countPossible towels patterns = do
  res <- mapM (waysToCreate' towels) patterns
  return $ length $ filter (0 <) res

countPossibleWays :: (Monad m, MonadState (Map Pattern Int) m) => [Towel] -> [Pattern] -> m Int
countPossibleWays towels patterns = do
  res <- mapM (waysToCreate' towels) patterns
  return $ sum res

waysToCreate' :: (Monad m, MonadState (Map Pattern Int) m) => [Towel] -> Pattern -> m Int
waysToCreate' = fix waysToCreate

waysToCreate :: (Monad m, MonadState (Map Pattern Int) m) => ([Towel] -> Pattern -> m Int) -> [Towel] -> Pattern -> m Int
waysToCreate _ _ "" = return 1
waysToCreate f towels pattern = do
  calculated <- gets (Data.Map.lookup pattern)
  case calculated of
    Just b -> return b
    Nothing -> do
      let validPrefixes = filter (`isPrefixOf` pattern) towels
      if null validPrefixes
        then do
          modify (insert pattern 0)
          return 0
        else do
          let validSuffixes = [p | prefix <- validPrefixes, let p = drop (length prefix) pattern]
          res <- mapM (f towels) validSuffixes
          let ways = sum res
          modify (insert pattern ways)
          return ways
