module Kainoa.Matrix
( openMatrix
, getIds
, getPops
) where

import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)

import Kainoa.Types
import Kainoa.IntsTbl (openIntsTbl, getIntsFromTbl)

openMatrix :: FilePath -> IO Matrix
openMatrix dir = do
  ids  <- openIntsTbl dir "mtx.0.ids"
  pops <- openIntsTbl dir "mtx.0.pops"
  return $ Matrix ids pops

getIds :: Matrix -> Int -> [Int]
getIds (Matrix ids _) id =
    getIntsFromTbl ids id

getPops :: Matrix -> Int -> [Int]
getPops (Matrix _ pops) id =
    getIntsFromTbl pops id
