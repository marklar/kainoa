module Kainoa.IntsIdxTbl
( openIntsIdxTbl
, getIntsFromIdxTbl
) where

import Kainoa.IntsTbl
import Kainoa.Types

openIntsIdxTbl :: FilePath -> FilePath -> IO IntsIdxTbl
openIntsIdxTbl dir root = do
  intsTbl <- openIntsTbl dir root
  let initIdx = head $ getIntsFromTbl intsTbl 1
  return $ IntsIdxTbl intsTbl initIdx

firstUsedIdx :: Int
firstUsedIdx = 2

getIntsFromIdxTbl :: IntsIdxTbl -> Int -> [Int]
getIntsFromIdxTbl (IntsIdxTbl ints initIdx) id =
    getIntsFromTbl ints realIdx
    where
      realIdx = id + firstUsedIdx - initIdx
      
