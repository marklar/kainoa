module Kainoa.IntsBL
( openIntsBL
, getLength
, getInt
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)

import Kainoa.Types
import Kainoa.Util.ByteString (readInt, substr)
import Kainoa.Util.Integral (toInt, toInt64, Int64)

-- Use this as a utility function,
-- from which to build others.
getIntsAt :: IntsBL -> Int -> Int -> [Int]
getIntsAt (IntsBL ints) off num = undefined

getInt :: IntsBL -> Int -> Maybe Int
getInt (IntsBL ints) id =
    case substr ints offset 4 of  -- bytesPerInt
      Just s -> Just (readInt s)
      Nothing -> Nothing
    where
      offset = toInt64 $ (id-1) * 4  -- bytesPerInt

getLength :: IntsBL -> Int
getLength (IntsBL pops) =
    toInt (BL.length pops) `div` 4  -- bytesPerInt

openIntsBL :: FilePath -> FilePath -> IO IntsBL
openIntsBL dir name = do
  liftM IntsBL $ unsafeMMapFile (dir ++ "/" ++ name)
