module Kainoa.Matrix
( openMatrix
, getIds
, getPops
) where

import qualified Data.Vector.Storable as V
import Control.Monad (liftM)
import Data.Int (Int32)

import Kainoa.Types
import Kainoa.IntsTbl (openIntsTbl, getIntsFromTbl)

openMatrix :: FilePath -> IO Matrix
openMatrix dir = do
  ids  <- openIntsTbl dir "mtx.0.ids"
  pops <- openIntsTbl dir "mtx.0.pops"
  return $ Matrix ids pops

getIds :: Matrix -> Int -> V.Vector Int32
getIds (Matrix ids _) id =
    getIntsFromTbl ids id

getPops :: Matrix -> Int -> V.Vector Int32
getPops (Matrix _ pops) id =
    getIntsFromTbl pops id
