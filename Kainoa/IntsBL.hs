module Kainoa.IntsBL
( openIntsBL
, getNumInts
, getInt
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)

import Kainoa.Types
import Kainoa.Util.ByteString (readInt, substr)
import Kainoa.Util.Integral (toInt, toInt64)


openIntsBL :: FilePath -> FilePath -> IO IntsBL
openIntsBL dir name = do
  liftM IntsBL $ unsafeMMapFile (dir ++ "/" ++ name)

getInt :: IntsBL -> Int -> Maybe Int
getInt (IntsBL ints) id =
    case substr ints offset 4 of  -- bytesPerInt
      Just s -> Just (readInt s)
      Nothing -> Nothing
    where
      offset = toInt64 $ id * 4  -- bytesPerInt

-- Store this in IntsBL type?
getNumInts :: IntsBL -> Int
getNumInts (IntsBL ints) =
    toInt (BL.length ints) `div` 4  -- bytesPerInt

-- Use this as a utility function,
-- from which to build others.
getIntsAt :: IntsBL -> Int -> Int -> [Int]
getIntsAt (IntsBL ints) off num = undefined
