module Main where

import System.Environment
import Data.List (intercalate, inits)
import Control.Parallel (par)

import Kainoa.Types
import Kainoa.Util.Charset (utf8ToLatin1)
import Kainoa.Domain (openDomain)
import Kainoa.Util.OrdList
import Kainoa.Query (makeQuery, evalQuery)

main = do
  lexemes <- getArgs
  let query = makeQuery (unwords lexemes)
  domains <- mapM openDomain ["idx/browse", "idx/games", "idx/music"]
  let resSet = map (evalQuery query) domains
  -- putStrLn . show $ parList resSet resSet
  putStrLn $ show resSet


parList :: [a] -> b -> b
parList []     b = b
parList (x:xs) b = x `par` parList xs b
