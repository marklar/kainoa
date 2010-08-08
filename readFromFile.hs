module Main where

import qualified Data.ByteString.Lazy as BL
import System.Environment
import Data.List (intercalate)

import Kainoa.Types
import Kainoa.Util.Charset (utf8ToLatin1)
import Kainoa.Domain (openDomain, getResults)
import Kainoa.Util.OrdList

main = do
  [lexeme] <- getArgs
  domain <- openDomain "idx/browse"
  putStrLn $ show (getResults domain lexeme)

  putStrLn . show $ ordMergeNub [1,2,3,4] [1,3,5,7]
  putStrLn . show $ ordIntersectNub [1,2,3,4] [1,3,5,7]
