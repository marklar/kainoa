module Main where

import qualified Data.ByteString.Lazy as BL
import System.Environment
import Data.List (intercalate)

import Kainoa.Types
import Kainoa.Util.Charset (utf8ToLatin1)
import Kainoa.Domain (openDomain, getResults)

main = do
  [lexeme] <- getArgs
  domain <- openDomain "idx/games"
  putStrLn . intercalate "\n" $ map show (getResults domain lexeme)
