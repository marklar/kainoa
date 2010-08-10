module Kainoa.Search
( search
) where

import System.Environment
import Control.Parallel (par)

import Kainoa.Types
import Kainoa.Util.Charset (utf8ToLatin1)
import Kainoa.Util.OrdList
import Kainoa.Query (makeQuery, evalQuery)

search :: [Domain] -> String -> [ResultSet]
search domains queryStr =    map (evalQuery query) domains
    where      query = makeQuery queryStr

parList :: [a] -> b -> b
parList []     b = b
parList (x:xs) b = x `par` parList xs b
