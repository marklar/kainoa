module Kainoa.Lexicon
( openLexicon
, getLexeme
) where

import Kainoa.StrTbl
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)

import Kainoa.Types
import Kainoa.IntsTbl (openIntsTbl, getIntsFromTbl)
import Kainoa.IntsBL (openIntsBL)
import Kainoa.StrTbl (openStrTbl, getStrFromTbl)

openLexicon :: FilePath -> IO Lexicon
openLexicon dir = do
  hd2id <- openIntsBL dir "lex.hd:id.data"
  strs  <- openStrTbl dir "lex"
  let maxId = strTblMaxId strs
  return $ Lexicon hd2id strs maxId

getLexeme :: Lexicon -> Int -> Maybe BL.ByteString
getLexeme (Lexicon head strs maxId) id =
    getStrFromTbl strs id

