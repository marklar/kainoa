module Kainoa.Domain
( openDomain
, getResults
) where

import qualified Data.Vector.Storable as V
import System.Environment
import Control.Monad (liftM)
import Data.List (concat, nub, sort)
import Data.Maybe (mapMaybe)
import Data.Int (Int32)

import Kainoa.Types
import Kainoa.ResultTbl (openResultTbl, getResult, getResultIdForPop)
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

getResults :: Domain -> String -> ResultSet
getResults domain@(Domain _ lexicon _ _ _) lexeme =
    ResultSet (shortResults ++ popResults)
    where
      shortResults = take 1  $ getShortResults domain lexemeIds
      popResults   = take 20 $ filter notShort $ getPopResults domain lexemeIds
      notShort r = r `notElem` shortResults
      lexemeIds = getLexemeIds lexicon lexeme

{-
  Currently, filtering it by 'notShort'.  In other words, don't repeat.
  However, also need to filter:
    + if faux result, and already seen (i.e. 'short')
    + if browse domain, and too few glus in stock.
      - get tags for result.  (theres a TagTbl for that.)
      - calc avail glus for those tags.
-}

getShortResults :: Domain -> [Int] -> [Result]
getShortResults (Domain _ _ matrix resultTbl _) lexemeIds =
    mapMaybe (getResult resultTbl) resultIds
    where
      resultIds = getResultIds (getIds matrix) lexemeIds

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
