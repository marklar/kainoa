module Kainoa.Domain
( openDomain
, getResults
) where

import qualified Data.ByteString.Lazy as BL
import System.Environment
import Control.Monad (liftM)
import Data.List (concat, nub, sort)
import Data.Maybe (mapMaybe)

import Kainoa.Types
import Kainoa.ResultTbl (openResultTbl, getResult, getResultIdForPop)
import Kainoa.Matrix (openMatrix, getIds, getPops)
import Kainoa.Lexicon (openLexicon, getLexemeIds)
import Kainoa.TagTbl (openTagTbl)

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
      shortResults = take 1 $ getShortResults domain lexemeIds
      popResults   = take 4 $ filter notShort $ getPopResults domain lexemeIds
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

-- FixMe.  Unperformant.
getResultIds :: (Int -> [Int]) -> [Int] -> [Int]
getResultIds postingsFun lexemeIds =
    sort . nub . concat $ map postingsFun lexemeIds
    
