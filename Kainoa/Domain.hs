module Kainoa.Domain
( openDomain
, getShortResults
, getPopResults
, getShortIds
, getPopIds
, getMaxResId
) where

import qualified Data.Vector.Storable as V
import System.Environment
import Control.Monad (liftM)
import Data.List (concat, nub, sort)
import Data.Maybe (mapMaybe)
import Data.Int (Int32)

import Kainoa.Types
import Kainoa.ResultTbl (openResultTbl, getResult, getResultIdForPop, getMaxId)
import Kainoa.Matrix (openMatrix, getIds, getPops)
import Kainoa.Lexicon (openLexicon, getLexemeIds)
import Kainoa.TagTbl (openTagTbl)
import Kainoa.Util.Integral (toInt)
import Kainoa.Util.OrdList (ordMergeNub)

openDomain :: FilePath -> IO Domain
openDomain dir = do
  lexicon   <- openLexicon   dir
  matrix    <- openMatrix    dir
  resultTbl <- openResultTbl dir
  tagTbl    <- openTagTbl    dir
  return $ Domain dir lexicon matrix resultTbl tagTbl

getMaxResId :: Domain -> Int
getMaxResId (Domain _ _ _ resultTbl _) =
    getMaxId resultTbl

------

getShortIds :: Domain -> [Int] -> [Int]
getShortIds (Domain _ _ matrix _ _) lexemeIds =
    getResultIds (getIds matrix) lexemeIds

getPopIds :: Domain -> [Int] -> [Int]
getPopIds (Domain _ _ matrix _ _) lexemeIds =
    getResultIds (getPops matrix) lexemeIds

------
    
-- For a single lexeme.
getShortResults :: Domain -> [Int] -> [Result]
getShortResults (Domain _ _ matrix resultTbl _) lexemeIds =
    mapMaybe (getResult resultTbl) resultIds
    where
      resultIds = getResultIds (getIds matrix) lexemeIds

-- For a single lexeme.
getPopResults :: Domain -> [Int] -> [Result]
getPopResults (Domain _ _ matrix resultTbl _) lexemeIds =
    mapMaybe (getResult resultTbl) resultIds
    where
      resultIds = mapMaybe (getResultIdForPop resultTbl) popIds
      popIds = getResultIds (getPops matrix) lexemeIds

getResultIds :: (Int -> V.Vector Int32) -> [Int] -> [Int]
getResultIds postingsFetcher lexemeIds =
    case resIdLists of
      [] -> []
      otherwise -> map toInt $ foldl1 ordMergeNub resIdLists
    where
      resIdLists = map (V.toList . postingsFetcher) lexemeIds
