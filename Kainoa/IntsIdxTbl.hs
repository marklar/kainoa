module Kainoa.IntsIdxTbl
( makeIntsIdxTbl
, getIntsFromIdxTbl
) where

import Kainoa.Util (toInt)
import Kainoa.IntsTbl
import Kainoa.Types

makeIntsIdxTbl :: FilePath -> FilePath -> IO IntsIdxTbl
makeIntsIdxTbl dir root = do
  intsTbl <- makeIntsTbl dir root
  let initIdx = head $ getIntsFromTbl intsTbl 1
  return $ IntsIdxTbl intsTbl initIdx

firstUsedIdx :: Int
firstUsedIdx = 2

getIntsFromIdxTbl :: IntsIdxTbl -> Int -> [Int]
getIntsFromIdxTbl (IntsIdxTbl ints initIdx) id =
    getIntsFromTbl ints realIdx
    where
      realIdx = id + firstUsedIdx - initIdx
      
