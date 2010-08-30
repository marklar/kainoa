module Kainoa.Query
( makeQuery
, evalQuery
) where

import Data.List (nub)
import Data.Maybe (mapMaybe)

import Kainoa.Types
import Kainoa.Util.OrdList (ordIntersectNubBy, ordMergeNubBy, ordDiffBy)
import Kainoa.Domain (getShortIds, getPopIds, getMaxResId)
import Kainoa.Lexicon (getLexemeIds)
import Kainoa.ResultTbl (getResult, getResultIdForPop, getMaxId)
import Data.Maybe (fromJust)


makeQuery :: String -> Query
makeQuery queryStr =
    case qLex queryStr of
      [] -> EmptyQuery
      lexemes -> andQueries $ map ofLexeme lexemes

{-
  ToDo:
    - filter Results based on:
       + those already seen (i.e. for pops, the one 'short').
       + if browse, number of Glus in stock for that tag.
    - insert short into top X pops, sorting by popularity.
    - create without-domain-name Query and ??  (backfill?)
-}
evalQuery :: Query -> Domain -> ResultSet
evalQuery q d@(Domain _ _ _ resultTbl _) =
    ResultSet (take 1 shortResults ++ take 20 popResults)
    where
      shortResults = mapRes shortResIds
      popResults = mapRes popResIds
      shortResIds = eval' lxmIdQ (getShortIds d) 0 maxId
      popResIds = mapMaybe (getResultIdForPop resultTbl) $
                  eval' lxmIdQ (getPopIds   d) 0 maxId
      mapRes = map (fromJust . getResult resultTbl)
      lxmIdQ = forDomain q d
      maxId = getMaxResId d

-------------

{-
  Convert from "string" version to "ids" version.
-}
forDomain :: Query -> Domain -> Query
forDomain EmptyQuery _ = EmptyQuery
forDomain (Not q) d    = Not (forDomain q d)
forDomain (And l r) d  = And (forDomain l d) (forDomain r d)
forDomain (Or  l r) d  = Or  (forDomain l d) (forDomain r d)
forDomain (Leaf (Lexeme s)) d@(Domain _ lexicon _ _ _) =
    Leaf (LexemeIds $ getLexemeIds lexicon s)
forDomain lf _ = lf
      
------

{-
  I think what's needed is a "next" fun.
  Returns a single Int, not [Int].
  Give me first value >X.

  For each value we want, we call
  the top-level function WITH A NEW minId,
  based on previous.
-}

eval' :: Query
         -> ([Int] -> [Int]) -- idFetcher (LxmIds -> PostingIds)
         -> Int  -- Min ID
         -> Int  -- Max ID   (make "bounds")
         -> [Int]

eval' EmptyQuery _ _ _ = []

eval' (And l r) fetch minId maxId =
    ordIntersectNubBy compare left right
    where 
      left   = eval' l fetch minId  maxId
      right  = eval' r fetch leftId maxId
      leftId = head left  -- could raise!

eval' (Or  l r) fetch minId maxId =
    ordMergeNubBy compare (e' l) (e' r)
    where e' q = eval' q fetch minId maxId

-- MODIFY HERE?
-- Faster if use minId?
eval' (Leaf (LexemeIds lxmIds)) fetch minId _ =
    dropWhile (< minId) (fetch lxmIds)

eval' (Not q) fetch minId maxId =
    ordDiffBy compare allIds notIds
    where
      allIds = [minId..maxId]
      notIds = eval' q fetch minId maxId

{-
  This shouldn't be necessary.
  Should already have converted tree to Ids version
  before attempting to eval.
-}
eval' strLeaf fetch minId maxId = undefined

{-
  Called 'qLex' because Prelude contains 'lex', too.
  Doesn't actually parse.  Just splits into words.
-}
qLex :: String -> [String]
qLex = nub . words

{-
  Just a simple placeholder.
  Does not account for variants, etc.
-}
ofLexeme :: String -> Query
ofLexeme = Leaf . Lexeme
    
andQueries :: [Query] -> Query
andQueries []     = EmptyQuery
andQueries (q:qs) = foldl And q qs
