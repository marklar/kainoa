module Kainoa.Domain
( makeDomain
, getText
, getTargets
, getResultsForTarget
) where

import qualified Data.ByteString.Lazy as BL

import Kainoa.Types
import Kainoa.StrTbl (makeStrTbl, getStrFromTbl)
import Kainoa.IntsTbl (makeIntsTbl, getIntsFromTbl)
import Kainoa.IntsIdxTbl (makeIntsIdxTbl, getIntsFromIdxTbl)


makeDomain :: FilePath -> IO Domain
makeDomain dir = do
  textTbl       <- makeStrTbl     dir "res.text"
  targetsTbl    <- makeIntsTbl    dir "res.target_ids"
  targetsIdxTbl <- makeIntsIdxTbl dir "res.target_ids.idx"
  return $ Domain textTbl targetsTbl targetsIdxTbl

getText :: Domain -> Int -> Maybe BL.ByteString
getText (Domain texts _ _) id =
    getStrFromTbl texts id

getTargets :: Domain -> Int -> [Int]
getTargets (Domain _ targetsTbl _) id =
    getIntsFromTbl targetsTbl id

getResultsForTarget :: Domain -> Int -> [Int]
getResultsForTarget d@(Domain _ _ targetsIdxTbl) targetId =
    getIntsFromIdxTbl targetsIdxTbl targetId
