module Day09 (task1, task2, parser) where

import Data.Char (digitToInt)
import Data.Map (lookupGE, lookupGT)
import Data.Map.Lazy (Map, delete, fromList, insert)
import Data.Text (Text, pack)
import Text.Parsec (digit, many)
import Text.Parsec.Text (Parser)
import Debug.Trace (trace)

newtype DiskDesc = Disk [Int] deriving (Show)

data DiskBlock = File Int Int | Empty Int deriving (Show)

data DiskBlockWithIndex = DiskBlockWithIndex DiskBlock Int deriving Show

task1 :: DiskDesc -> Text
task1 = pack . show . processMap . map (\(DiskBlockWithIndex b _) -> b) . createBlockList

task2 :: DiskDesc -> Text
task2 = pack . show . calculateChecksum . reshuffleFiles . createBlockList

createBlockList :: DiskDesc -> [DiskBlockWithIndex]
createBlockList d = createBlockList' d 0 0

createBlockList' :: DiskDesc -> Int -> Int -> [DiskBlockWithIndex]
createBlockList' (Disk (f : e : bs)) fileId index = DiskBlockWithIndex (File f fileId) index : DiskBlockWithIndex (Empty e) (f + index) : createBlockList' (Disk bs) (fileId + 1) (f + e + index)
createBlockList' (Disk [f]) fileId index = [DiskBlockWithIndex (File f fileId) index]
createBlockList' (Disk []) _ _ = []

data Direction = Front | Back

data Offset = FrontOffset Int | BackOffset Int

processMap :: [DiskBlock] -> Int
processMap blocks = processMap' blocks (reverse blocks) 0 (BackOffset 0) Front

processMap' :: [DiskBlock] -> [DiskBlock] -> Int -> Offset -> Direction -> Int
processMap' (File size file1Id : bs) bw@(File _ file2Id : _) index offset@(BackOffset backOffset) Front
  | file1Id /= file2Id = blockValue file1Id size index + processMap' bs bw (index + size) offset Back
  | otherwise = blockValue file1Id (size - backOffset) index
processMap' forwardMap (Empty _ : bs) index offset Back = processMap' forwardMap bs index offset Back
processMap' fw@(Empty emptySize : fbs) bw@(File fileSize fileId : bbs) index (FrontOffset frontOffset) Back
  | (emptySize - frontOffset) >= fileSize = blockValue fileId fileSize index + processMap' fw bbs (index + fileSize) (FrontOffset (frontOffset + fileSize)) Back
  | otherwise = blockValue fileId (emptySize - frontOffset) index + processMap' fbs bw (index + (emptySize - frontOffset)) (BackOffset (emptySize - frontOffset)) Front
processMap' fw@(Empty emptySize : fbs) bw@(File fileSize fileId : bbs) index (BackOffset backOffset) Back
  | (fileSize - backOffset) >= emptySize = blockValue fileId emptySize index + processMap' fbs bw (index + emptySize) (BackOffset (backOffset + emptySize)) Front
  | otherwise = blockValue fileId (fileSize - backOffset) index + processMap' fw bbs (index + (fileSize - backOffset)) (FrontOffset (fileSize - backOffset)) Back
processMap' _ _ _ _ _ = undefined

blockValue :: (Num a, Enum a) => a -> a -> a -> a
blockValue fileId size startIndex = sum (map ((* fileId) <$> (+ startIndex)) [0 .. size - 1])

parser :: Parser DiskDesc
parser = do
  digits <- many digit
  let numbers = map digitToInt digits
  return $ Disk numbers

data EmptySpace = EmptySpace Int Int deriving Show

instance Eq EmptySpace where
  (EmptySpace s1 o1) == (EmptySpace s2 o2) = s1 == s2 && o1 == o2

instance Ord EmptySpace where
  compare (EmptySpace size1 offset1) (EmptySpace size2 offset2) = compare size1 size2 <> compare offset1 offset2

createEmptySpaceMap :: [DiskBlockWithIndex] -> Map EmptySpace ()
createEmptySpaceMap blocks = fromList [(EmptySpace size offset, ()) | DiskBlockWithIndex (Empty size) offset <- blocks, size /= 0]

createUsedSpaceList :: [DiskBlockWithIndex] -> [DiskBlockWithIndex]
createUsedSpaceList blocks = [l | l@(DiskBlockWithIndex (File _ _) _) <- blocks]

reshuffleFiles :: [DiskBlockWithIndex] -> [DiskBlockWithIndex]
reshuffleFiles blocks =
  let fileList = (reverse . createUsedSpaceList) blocks
      spaceMap = createEmptySpaceMap blocks
   in reshuffleFiles' fileList spaceMap

reshuffleFiles' :: [DiskBlockWithIndex] -> Map EmptySpace () -> [DiskBlockWithIndex]
reshuffleFiles' [] spaces = trace (show spaces) []
reshuffleFiles' (f@(DiskBlockWithIndex (File size fileId) _) : fs) spaceMap =
  let maybeEmptySpace = lookupGT (EmptySpace size 0) spaceMap
   in case maybeEmptySpace of
        Nothing -> f : reshuffleFiles' fs spaceMap
        Just (es@(EmptySpace spaceSize emptySpaceIndex), ()) -> DiskBlockWithIndex (File size fileId) emptySpaceIndex : reshuffleFiles' fs (updateMap spaceMap) where
          updateMap m 
            | spaceSize == size = delete es m
            | otherwise = insert (EmptySpace (spaceSize - size) (emptySpaceIndex + size)) () (delete es spaceMap)
reshuffleFiles' _ _ = undefined

calculateChecksum :: [DiskBlockWithIndex] -> Int
calculateChecksum =  sum . map checksum
  where
    checksum (DiskBlockWithIndex (Empty _) _) = 0
    checksum (DiskBlockWithIndex (File size fileId) index) = blockValue fileId size index