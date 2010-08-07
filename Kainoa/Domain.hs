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

openDomain :: FilePath -> IO Domain
openDomain dir = do
  lexicon   <- openLexicon dir
  matrix    <- openMatrix dir
  resultTbl <- openResultTbl dir
  return $ Domain dir lexicon matrix resultTbl

getResults :: Domain -> String -> [Result]
getResults domain@(Domain _ lexicon _ _) lexeme =
    shortResults ++ popResults
    where
      shortResults = take 1 $ getShortResults domain lexemeIds
      popResults   = take 4 $ filter notShort $ getPopResults domain lexemeIds
      notShort r = r `notElem` shortResults
      lexemeIds = getLexemeIds lexicon lexeme

getShortResults :: Domain -> [Int] -> [Result]
getShortResults (Domain _ _ matrix resultTbl) lexemeIds =
    mapMaybe (getResult resultTbl) resultIds
    where
      resultIds = getResultIds (getIds matrix) lexemeIds

getPopResults :: Domain -> [Int] -> [Result]
getPopResults (Domain _ _ matrix resultTbl) lexemeIds =
    mapMaybe (getResult resultTbl) resultIds
    where
      resultIds = mapMaybe (getResultIdForPop resultTbl) popIds
      popIds = getResultIds (getPops matrix) lexemeIds

-- FixMe.  Unperformant.
getResultIds :: (Int -> [Int]) -> [Int] -> [Int]
getResultIds postingsFun lexemeIds =
    sort . nub . concat $ map postingsFun lexemeIds
    
