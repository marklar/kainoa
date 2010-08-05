module Kainoa.IntsTbl
( makeIntsTbl
, getIntsFromTbl
, firstOffset
, IntsTbl
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Control.Monad (liftM)

import Kainoa.Offsets (dataOffAndLen, makeOffsets, offsetForIdx)
import Kainoa.Util (getInts, toInt)
import Kainoa.Types


makeIntsTbl :: FilePath -> FilePath -> IO IntsTbl
makeIntsTbl dir root =
    do let prefix = dir ++ "/" ++ root
       offs <- liftM makeOffsets $ unsafeMMapFile (prefix ++ ".offs")
       ints <- liftM Ints        $ unsafeMMapFile (prefix ++ ".data")
       return $ IntsTbl offs ints

getIntsFromTbl :: IntsTbl -> Int -> [Int]
getIntsFromTbl (IntsTbl offs (Ints intsBl)) idx = 
    case dataOffAndLen offs idx of
      (Nothing, _) -> []
      (Just off, mLen) -> getInts intsBl off ((toInt len) `div` 4)
          where len = case mLen of
                        Just l -> l
                        Nothing -> (BL.length intsBl) - off

firstOffset :: IntsTbl -> Maybe Int64
firstOffset (IntsTbl offs dat) =
    offsetForIdx offs 0
