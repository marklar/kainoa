module Kainoa.IntsTbl
( openIntsTbl
, getIntsFromTbl
, firstOffset
, IntsTbl
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Control.Monad (liftM)

import Kainoa.Offsets (dataOffAndLen, openOffsets, offsetForIdx)
import Kainoa.Util (getInts, toInt)
import Kainoa.Types


openIntsTbl :: FilePath -> FilePath -> IO IntsTbl
openIntsTbl dir root =
    do let prefix = dir ++ "/" ++ root
       offs <- liftM openOffsets $ unsafeMMapFile (prefix ++ ".offs")
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
