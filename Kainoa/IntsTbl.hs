module Kainoa.IntsTbl
( openIntsTbl
, getIntsFromTbl
, firstOffset
, IntsTbl
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)

import Kainoa.Offsets (dataOffAndLen, offsetForIdx)
import Kainoa.Util.ByteString (readInts)
import Kainoa.Util.Integral (toInt, Int64)
import Kainoa.Types


openIntsTbl :: FilePath -> FilePath -> IO IntsTbl
openIntsTbl dir root =
    do let prefix = dir ++ "/" ++ root
       offs <- liftM Offsets $ unsafeMMapFile (prefix ++ ".offs")
       ints <- liftM IntsBL  $ unsafeMMapFile (prefix ++ ".data")
       return $ IntsTbl offs ints

bytesPerInt :: Int
bytesPerInt = 4

getIntsFromTbl :: IntsTbl -> Int -> [Int]
getIntsFromTbl (IntsTbl offs (IntsBL intsBL)) idx = 
    case dataOffAndLen offs idx of
      (Nothing, _) -> []
      (Just off, mLen) -> readInts intsBL off numInts
          where
            numInts = (toInt len) `div` bytesPerInt
            len = case mLen of
                    Just l -> l
                    Nothing -> (BL.length intsBL) - off

firstOffset :: IntsTbl -> Maybe Int64
firstOffset (IntsTbl offs dat) =
    offsetForIdx offs 0
