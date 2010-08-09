module Kainoa.ResultTbl
( openResultTbl
, getResult
, getPop
, getResultIdForPop
, getText
, getTargets
, getResultsForTarget
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.Vector.Storable as V
import Control.Monad (liftM)
import Data.Int (Int32)

import Kainoa.Types
import Kainoa.StrTbl (openStrTbl, getStrFromTbl)
import Kainoa.IntsTbl (openIntsTbl, getIntsFromTbl)
import Kainoa.IntsIdxTbl (openIntsIdxTbl, getIntsFromIdxTbl)
import Kainoa.IntsV (openIntsV, getNumInts, getInt)
import Kainoa.BoolV (openBoolV)


openResultTbl :: FilePath -> IO ResultTbl
openResultTbl dir = do
  pops          <- openIntsV      dir "res.pop.data"
  popsIdx       <- openIntsV      dir "res.pop.idx.data"
  textTbl       <- openStrTbl     dir "res.text"
  targetsTbl    <- openIntsTbl    dir "res.target_ids"
  targetsIdxTbl <- openIntsIdxTbl dir "res.target_ids.idx"
  isFauxV       <- openBoolV      dir "res.is_faux.data"
  return $ ResultTbl pops popsIdx textTbl targetsTbl targetsIdxTbl isFauxV (getNumInts pops)

getResult :: ResultTbl -> Int -> Maybe Result
getResult resultTbl id =
    case getPop resultTbl id of
      Nothing  -> Nothing
      Just pop -> Just $ Result id pop text targetIds
    where targetIds = getTargets resultTbl id
          text = getText resultTbl id

getPop :: ResultTbl -> Int -> Maybe Int
getPop (ResultTbl pops _ _ _ _ _ _) id =
    getInt pops (id-1)

getResultIdForPop :: ResultTbl -> Int -> Maybe Int
getResultIdForPop (ResultTbl _ popsIdx _ _ _ _ _) pop =
    getInt popsIdx (pop-1)

getText :: ResultTbl -> Int -> String
getText (ResultTbl _ _ texts _ _ _ _) id =
    getStrFromTbl texts id

getTargets :: ResultTbl -> Int -> V.Vector Int32
getTargets (ResultTbl _ _ _ targetsTbl _ _ _) id =
    getIntsFromTbl targetsTbl id

getResultsForTarget :: ResultTbl -> Int -> V.Vector Int32
getResultsForTarget (ResultTbl _ _ _ _ targetsIdxTbl _ _) targetId =
    getIntsFromIdxTbl targetsIdxTbl targetId
