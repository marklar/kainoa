module Kainoa.ResultTbl
( openResultTbl
, getPop
, getResultIdForPop
, getText
, getTargets
, getResultsForTarget
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)

import Kainoa.Types
import Kainoa.StrTbl (openStrTbl, getStrFromTbl)
import Kainoa.IntsTbl (openIntsTbl, getIntsFromTbl)
import Kainoa.IntsIdxTbl (openIntsIdxTbl, getIntsFromIdxTbl)
import Kainoa.IntsBL (openIntsBL, getLength, getInt)


openResultTbl :: FilePath -> IO ResultTbl
openResultTbl dir = do
  pops          <- openIntsBL     dir "res.pop.data"
  popsIdx       <- openIntsBL     dir "res.pop.idx.data"
  textTbl       <- openStrTbl     dir "res.text"
  targetsTbl    <- openIntsTbl    dir "res.target_ids"
  targetsIdxTbl <- openIntsIdxTbl dir "res.target_ids.idx"
  return $ ResultTbl pops popsIdx textTbl targetsTbl targetsIdxTbl (getLength pops)

getPop :: ResultTbl -> Int -> Maybe Int
getPop (ResultTbl pops _ _ _ _ _) id =
    getInt pops id

getResultIdForPop :: ResultTbl -> Int -> Maybe Int
getResultIdForPop (ResultTbl _ popsIdx _ _ _ _) pop =
    getInt popsIdx pop

getText :: ResultTbl -> Int -> Maybe BL.ByteString
getText (ResultTbl _ _ texts _ _ _) id =
    getStrFromTbl texts id

getTargets :: ResultTbl -> Int -> [Int]
getTargets (ResultTbl _ _ _ targetsTbl _ _) id =
    getIntsFromTbl targetsTbl id

getResultsForTarget :: ResultTbl -> Int -> [Int]
getResultsForTarget (ResultTbl _ _ _ _ targetsIdxTbl _) targetId =
    getIntsFromIdxTbl targetsIdxTbl targetId
