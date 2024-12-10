module Day10 (parser, task1, task2) where

import Data.Char (digitToInt)
import Data.Graph (Graph, Vertex, graphFromEdges, reachable, vertices)
import Data.Matrix (Matrix (ncols, nrows), fromLists, safeGet, (!))
import Data.Text (pack, Text)
import Text.Parsec (newline, sepBy, space, many)
import Text.Parsec.Char (digit)
import Text.Parsec.Text (Parser)
import Debug.Trace (trace)


type Map = Matrix Int

parser :: Parser Map
parser = do
  lines <- sepBy lineParser newline
  return $ fromLists lines

lineParser :: Parser [Int]
lineParser = do
  elems <- many digit 
  return (map digitToInt elems)

getReachableNines :: (Eq b, Num b, Show b) => Matrix b -> Int
getReachableNines hikingMap = (length . concatMap (filter ((9 ==) . getVertexHeight) . reachable g) . filter ((0 ==) . getVertexHeight)) (vertices g)
  where
    (g, vertexGetter, _) = createGraph hikingMap
    getVertexHeight v = height
      where
        (height, _, _) = vertexGetter v

task1 :: Map -> Text
task1 = pack . show . getReachableNines 

task2 = undefined

dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

createGraph :: (Eq node, Num node, Show node) => Matrix node -> (Graph, Vertex -> (node, (Int, Int), [(Int, Int)]), (Int, Int) -> Maybe Vertex)
createGraph m = graphFromEdges edgesList
  where
    edgesList = [(height, (x, y), [(nx + x, ny + y) | (nx, ny) <- dirs, let nHeight = safeGet (nx + x) (ny + y) m, isValidTransition nHeight height]) | x <- [1 .. ncols m], y <- [1 .. nrows m], let height = m ! (x, y)]
      where
        isValidTransition Nothing _ = False
        isValidTransition (Just h2) h1 = h1 + 1 == h2