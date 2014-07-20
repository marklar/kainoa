{- |

Kainoa may search over multiple indices, each of which corresponds to
some set of things we may wish to search over.

Typically, there's one index per product vertical, e.g.

  + the titles of DVDs (e.g. "Emergency on Planet Earth"), or
  + the names of smartphones (e.g. "Apple iPhone 4")

But there are also indices for such things as:

  + names of charities (to which to donate), or
  + names of categories of products (e.g. "hockey games")

A Domain represents a single index, which is composed of multiple
parts (each described in more detail elsewhere):

  + Lexicon : find matching terms
  + Matrix : for terms, find matching results
  + ResultTable : for matching results, get corresponding data
  + TagTbl : addition result information, corresponding to MySQL DB data

The Domain module has two principle jobs:
  1. to provide tables for calculating a list of result IDs, and
  2. to filter the results for uniqueness of referent to create
     a displayable result set.

-}

module Kainoa.Domain
( openDomain
, getMaxResId
, getShortIds
, getPopIds
) where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Generic.Mutable as GV
import System.Environment
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

{-
  Instead of returning [Int],
  merge the multiple (V.Vector Int32)s
  into a single Mutable Vector.
  http://hackage.haskell.org/packages/archive/vector/0.5/doc/html/Data-Vector-Storable-Mutable.html
  The function 'write' allows one to replace a value
  at a given spot.
-}

getShortIds :: Domain -> [Int] -> [Int]
getShortIds (Domain _ _ matrix _ _) lexemeIds =
    getResultIds (getIds matrix) lexemeIds

getPopIds :: Domain -> [Int] -> [Int]
getPopIds (Domain _ _ matrix _ _) lexemeIds =
    getResultIds (getPops matrix) lexemeIds

------

getResultIds :: (Int -> V.Vector Int32) -> [Int] -> [Int]
getResultIds postingsFetcher lexemeIds =
    case resIdLists of
      [] -> []
      otherwise -> map toInt $ foldl1 ordMergeNub resIdLists
    where
      resIdLists = map (V.toList . postingsFetcher) lexemeIds

{-
  When doing a query like 's xy', where neither token requires merging,
  but where 's' has many postings and 'xy' has very few,
  the query takes a long time.  So, it's not a problem with merging.
  It's a problem of having to go through all the 's' postings
  while intersecting, and that takes a long time.

  getIds matrix lxmId
    returns a Vector of postings, lazily
    if you apply V.toList, then does it create the list lazily (or non-)?

  So where is it spending time?
  How to make this faster?
-}

{-
-- Non-lazily create union?  Wasted cycles?
getResultIds2 :: (Int -> V.Vector Int32) -> [Int] -> GV.Vector Int32
getResultIds2 postingsFetcher lexemeIds = do
  case resIdLists of
    [] -> GV.new 0
    otherwise -> do
        v <- GV.new maxLen
        merge v 0
        GV.unsafeFreeze v
      where
        resIdVectors = map postingsFetcher lexemeIds
        fill v i = do
                    let x = nextValue in
                    GV.unsafeWrite v i x
                    fill v (i+1)
-}