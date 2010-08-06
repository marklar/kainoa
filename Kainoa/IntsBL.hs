module Kainoa.IntsBL
( openIntsBL
, getLength
, getInt
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)

import Kainoa.Types
import Kainoa.Util (toInt, toInt64, readInt, substr)

getInt :: IntsBL -> Int -> Maybe Int
getInt (IntsBL ints) id =
    case substr ints offset 4 of
      Just s -> Just (readInt s)
      Nothing -> Nothing
    where
      offset = toInt64 $ (id-1) * 4

getLength :: IntsBL -> Int
getLength (IntsBL pops) =
    toInt (BL.length pops) `div` 4

openIntsBL :: FilePath -> FilePath -> IO IntsBL
openIntsBL dir name = do
  liftM IntsBL $ unsafeMMapFile (dir ++ "/" ++ name)
