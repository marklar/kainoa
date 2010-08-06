module Kainoa.Lexicon
( openLexicon
, getLexeme
, getLexeme'
, findId
) where

import Kainoa.StrTbl
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)
import Char (ord)
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Kainoa.Types
import Kainoa.IntsTbl (openIntsTbl, getIntsFromTbl)
import Kainoa.IntsBL (openIntsBL, getInt)
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

getLexeme' :: Lexicon -> Int -> Maybe String
getLexeme' lexicon id =
    case getLexeme lexicon id of
      Nothing -> Nothing
      Just s -> Just $ unpack s

findId :: Lexicon -> String -> LexemeIdMatch
findId lexicon lexeme =
    case boundIds lexicon (ord $ head lexeme) of
      Nothing -> Miss
      Just (beg, aft) ->
          case getLexeme' lexicon beg of
            Nothing -> Miss  -- RAISE!
            Just s | s == lexeme -> Complete beg
            otherwise -> 
                case chopId lexicon lexeme beg aft of
                  After id -> Partial (id,aft)
                  At id    -> Complete id

---------------

data NearestId = At Int | After Int
                 deriving (Show)

data LexemeIdMatch = Miss
                   | Complete Int
                   | Partial (Int,Int)
                     deriving (Show)

boundIds :: Lexicon -> Int -> Maybe (Int, Int)
boundIds lexicon charCode =
    case firstIdForHead lexicon charCode of
      0  -> Nothing
      id -> Just (id, idAfter lexicon charCode)

firstIdForHead :: Lexicon -> Int -> Int
firstIdForHead (Lexicon hd2id _ _) charCode =
    case getInt hd2id charCode of  -- use 'maybe'
      Nothing -> 0
      Just id -> id

idAfter :: Lexicon -> Int -> Int
idAfter lexicon@(Lexicon hd2id strs maxId) charCode =
    if charCode >= maxCharCode then
        maxId + 1
    else
        case firstIdForHead lexicon nextCharCode of
          0  -> idAfter lexicon nextCharCode
          id -> id
    where
      nextCharCode = charCode + 1

maxCharCode = 255

chopId :: Lexicon -> String -> Int -> Int -> NearestId
chopId lexicon lexeme min max =
    if min > max then
        After min
    else
        case getLexeme' lexicon mid of
          Nothing -> After min  -- SHOULDN'T HAPPEN
          Just s ->
              case compare lexeme s of
                EQ -> At mid
                GT -> chopId lexicon lexeme (mid+1) max
                LT -> chopId lexicon lexeme min     (mid-1)
    where mid = (min + max) `div` 2
