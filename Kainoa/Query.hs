module Kainoa.Query
( makeQuery
, evalQuery
) where

import Data.List (nub)

import Kainoa.Types
import Kainoa.Util.OrdList (ordIntersectNubBy, ordMergeNubBy)
import Kainoa.Domain (getShortResults, getPopResults)
import Kainoa.Lexicon (getLexemeIds)
import Kainoa.Result (cmpId, cmpPop)


makeQuery :: String -> Query
makeQuery queryStr =
    case qLex queryStr of
      [] -> EmptyQuery
      lexemes -> andQueries $ map ofLexeme lexemes


evalQuery :: Query -> Domain -> ResultSet
evalQuery q d =
    ResultSet (take 1 shortResults ++ take 20 popResults)
    where
      shortResults = eval' idQ (getShortResults d) cmpId
      popResults   = eval' idQ (getPopResults   d) cmpPop
      idQ = forDomain q d

-------------

forDomain :: Query -> Domain -> Query
forDomain EmptyQuery _ = EmptyQuery
forDomain (Not q) d    = Not (forDomain q d)
forDomain (And l r) d  = And (forDomain l d) (forDomain r d)
forDomain (Or  l r) d  = Or  (forDomain l d) (forDomain r d)
forDomain (Leaf (Lexeme s)) d@(Domain _ lexicon _ _ _) =
    Leaf (LexemeIds $ getLexemeIds lexicon s)
      
eval' :: Query
         -> ([Int] -> [Result])              -- fetch postings
         -> (Result -> Result -> Ordering)   -- order Results
         -> [Result]
eval' EmptyQuery _ _ = []
eval' (And l r) fetch cmp =
    ordIntersectNubBy cmp (eval' l fetch cmp) (eval' r fetch cmp)
eval' (Or  l r) fetch cmp =
    ordMergeNubBy     cmp (eval' l fetch cmp) (eval' r fetch cmp)
eval' (Leaf (LexemeIds lxmIds)) fetch _ = fetch lxmIds
eval' (Not q)   fetch cmp = undefined

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
