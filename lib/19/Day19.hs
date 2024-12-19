module Day19 (parser, task1, task2) where

import Control.Monad.State (gets, modify, runState)
import Control.Monad.State.Lazy (MonadState)
import Data.Foldable.Extra (anyM)
import Data.Function (fix)
import Data.List (isPrefixOf)
import Data.Map (empty, insert, lookup)
import Data.Map.Strict (Map)
import Data.Text (Text, pack)
import Text.Parsec (newline, oneOf, sepBy, string)
import Text.Parsec.Prim (many)
import Text.Parsec.Text (Parser)
import Debug.Trace (trace)

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

task2 = undefined

towelsParser :: Parser [Towel]
towelsParser = many (oneOf ['w', 'u', 'b', 'r', 'g']) `sepBy` string ", "

patternParser :: Parser [Pattern]
patternParser = many (oneOf ['w', 'u', 'b', 'r', 'g']) `sepBy` newline

countPossible :: (Monad m, MonadState (Map Pattern Bool) m) => [Towel] -> [Pattern] -> m Int
countPossible towels patterns = do
  res <- mapM (canBeCreated' towels) patterns
  return $ length $ filter id res

canBeCreated' :: (Monad m, MonadState (Map Pattern Bool) m) => [Towel] -> Pattern -> m Bool
canBeCreated' = fix canBeCreated

canBeCreated :: (Monad m, MonadState (Map Pattern Bool) m) => ([Towel] -> Pattern -> m Bool) -> [Towel] -> Pattern -> m Bool
canBeCreated _ _ "" = return True
canBeCreated f towels pattern = do
  calculated <- gets (Data.Map.lookup pattern)
  case calculated of
    Just b -> return b
    Nothing -> do
      let validPrefixes = filter (`isPrefixOf` pattern) towels
      if null validPrefixes then
        do
            modify (insert pattern False)
            return False
        else do
            let validSuffixes = [p | prefix <- validPrefixes, let p = drop (length prefix) pattern]
            res <- anyM (f towels) validSuffixes
            modify (insert pattern res)
            return res

