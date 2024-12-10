module Day10 (parser, task1, task2) where

import Data.Char (digitToInt)
import Data.Graph (Graph, Vertex, graphFromEdges, reachable, reverseTopSort, vertices)
import Data.Map qualified as M (Map, empty, foldrWithKey, insert, (!))
import Data.Matrix (Matrix (ncols, nrows), fromLists, safeGet, (!))
import Data.Text (Text, pack)
import Text.Parsec (many, newline, sepBy)
import Text.Parsec.Char (digit)
import Text.Parsec.Text (Parser)

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
    (g, nodeGetter, _) = createGraph hikingMap
    getVertexHeight v = height
      where
        (height, _, _) = nodeGetter v

getNumPaths :: (Eq b, Num b, Show b) => Matrix b -> Int
getNumPaths hikingMap = M.foldrWithKey updateCounter 0 numpathsToEnd
  where
    (g, nodeGetter, vertexGetter) = createGraph hikingMap
    numpathsToEnd :: M.Map Vertex Int = foldl updatePathCounts M.empty (reverseTopSort g)
    updatePathCounts pathCounts vertex = case getVertexHeight vertex of
      9 -> M.insert vertex 1 pathCounts
      _ -> M.insert vertex (sumUpPathsFromVertex vertex) pathCounts
      where
        sumUpPathsFromVertex v = sum $ map pathsViaNeighbor (getVertexNeighbors v)
          where
            pathsViaNeighbor n = case vertexGetter n of
              Nothing -> 0
              Just neighborVertex -> pathCounts M.! neighborVertex
    updateCounter vertex totalPaths paths = case getVertexHeight vertex of
      0 -> paths + totalPaths
      _ -> paths

    getVertexHeight v = height
      where
        (height, _, _) = nodeGetter v
    getVertexNeighbors v = neighbors
      where
        (_, _, neighbors) = nodeGetter v

task1 :: Map -> Text
task1 = pack . show . getReachableNines

task2 :: Map -> Text
task2 = pack . show . getNumPaths

dirs :: [(Int, Int)]
dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

createGraph :: (Eq node, Num node, Show node) => Matrix node -> (Graph, Vertex -> (node, (Int, Int), [(Int, Int)]), (Int, Int) -> Maybe Vertex)
createGraph m = graphFromEdges edgesList
  where
    edgesList = [(height, (x, y), [(nx + x, ny + y) | (nx, ny) <- dirs, let nHeight = safeGet (nx + x) (ny + y) m, isValidTransition nHeight height]) | x <- [1 .. ncols m], y <- [1 .. nrows m], let height = m ! (x, y)]
      where
        isValidTransition Nothing _ = False
        isValidTransition (Just h2) h1 = h1 + 1 == h2