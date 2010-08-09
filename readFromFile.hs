module Main where

import System.Environment
import Data.List (intercalate, inits)

import Kainoa.Types
import Kainoa.Util.Charset (utf8ToLatin1)
import Kainoa.Domain (openDomain, getResults)
import Kainoa.Util.OrdList

main = do
  [lexeme] <- getArgs
  domain <- openDomain "idx/browse"
  -- putStrLn . show $ getResults domain lexeme
  mapM_ (\s -> putStrLn $ "\n" ++ show (getResults domain s)) (tail (inits lexeme))
