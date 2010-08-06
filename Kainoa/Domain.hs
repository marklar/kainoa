module Kainoa.Domain
( openDomain
) where

import qualified Data.ByteString.Lazy as BL
import System.Environment
import Control.Monad (liftM)
import Data.List (concat, nub)

import Kainoa.Types
import Kainoa.ResultTbl
    ( openResultTbl, getPop, getResultIdForPop
    , getText, getTargets, getResultsForTarget
    )
import Kainoa.Matrix (openMatrix, getIds, getPops)
import Kainoa.Lexicon (openLexicon, getLexeme', findId, ids)


openDomain :: FilePath -> IO Domain
openDomain dir = do
  lexicon   <- openLexicon dir
  matrix    <- openMatrix dir
  resultTbl <- openResultTbl dir
  return $ Domain dir lexicon matrix resultTbl

