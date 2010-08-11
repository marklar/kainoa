module Kainoa.Query
( makeQuery
, evalQuery
) where

import Data.List (nub)
import Data.Maybe (mapMaybe)

import Kainoa.Types
import Kainoa.Util.OrdList (ordIntersectNubBy, ordMergeNubBy, ordDiffBy)
import Kainoa.Domain (getShortResults, getPopResults)
import Kainoa.Lexicon (getLexemeIds)
import Kainoa.Result (cmpId, cmpPop)
import Kainoa.ResultTbl (getResult, getMaxId)


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
evalQuery q d =
    ResultSet (take 1 shortResults ++ take 20 popResults)
    where
      shortResults = eval' idQ getShortResults d cmpId
      popResults   = eval' idQ getPopResults   d cmpPop
      idQ = forDomain q d

-------------

forDomain :: Query -> Domain -> Query
forDomain EmptyQuery _ = EmptyQuery
forDomain (Not q) d    = Not (forDomain q d)
forDomain (And l r) d  = And (forDomain l d) (forDomain r d)
forDomain (Or  l r) d  = Or  (forDomain l d) (forDomain r d)
forDomain (Leaf (Lexeme s)) d@(Domain _ lexicon _ _ _) =
    Leaf (LexemeIds $ getLexemeIds lexicon s)
forDomain lf _ = lf
      
eval' :: Query
         -> (Domain -> [Int] -> [Result])    -- fetch postings
         -> Domain
         -> (Result -> Result -> Ordering)   -- order Results
         -> [Result]

eval' EmptyQuery _ _ _ = []

eval' (And l r) fetch dom cmp =
    ordIntersectNubBy cmp (e' l) (e' r)
    where e' q = eval' q fetch dom cmp

eval' (Or  l r) fetch dom cmp =
    ordMergeNubBy cmp (e' l) (e' r)
    where e' q = eval' q fetch dom cmp

eval' (Leaf (LexemeIds lxmIds)) fetch dom _ =
    fetch dom lxmIds

eval' (Not q) fetch dom@(Domain _ _ _ resultTbl _) cmp =
    ordDiffBy cmp allRes notRes
    where
      allRes = mapMaybe (getResult resultTbl) [1..maxId]
      notRes = eval' q fetch dom cmp
      maxId  = getMaxId resultTbl

eval' strLeaf fetch dom cmp =
    eval' idsLeaf fetch dom cmp
    where idsLeaf = forDomain strLeaf dom

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
