module Kainoa.IntsIdxTbl
( openIntsIdxTbl
, getIntsFromIdxTbl
) where

import qualified Data.Vector.Storable as V
import Data.Int (Int32)

import Kainoa.IntsTbl
import Kainoa.Types
import Kainoa.Util.Integral

openIntsIdxTbl :: FilePath -> FilePath -> IO IntsIdxTbl
openIntsIdxTbl dir root = do
  intsTbl <- openIntsTbl dir root
  let initIdx = toInt $ V.head $ getIntsFromTbl intsTbl 1
  return $ IntsIdxTbl intsTbl initIdx

firstUsedIdx :: Int
firstUsedIdx = 2

getIntsFromIdxTbl :: IntsIdxTbl -> Int -> V.Vector Int32
getIntsFromIdxTbl (IntsIdxTbl ints initIdx) id =
    getIntsFromTbl ints realIdx
    where
      realIdx = id + firstUsedIdx - initIdx
      
