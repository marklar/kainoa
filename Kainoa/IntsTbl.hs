module Kainoa.IntsTbl
( openIntsTbl
, getIntsFromTbl
, firstOffset
, IntsTbl
) where

import Data.Vector.Storable.MMap (unsafeMMapVector)
import qualified Data.Vector.Storable as V
import Control.Monad (liftM)
import Data.Int (Int32)

import Kainoa.Offsets (dataOffAndLen, offsetForIdx, offsetsMaxId)
import Kainoa.Util.ByteString (readInts)
import Kainoa.Util.Integral (toInt)
import Kainoa.Types


openIntsTbl :: FilePath -> FilePath -> IO IntsTbl
openIntsTbl dir root = do
  offs <- liftM Offsets $ unsafeMMapVector (prefix ++ ".offs") Nothing
  ints <- liftM IntsV   $ unsafeMMapVector (prefix ++ ".data") Nothing
  return $ IntsTbl offs ints (offsetsMaxId offs)
    where
      prefix = dir ++ "/" ++ root

getIntsFromTbl :: IntsTbl -> Int -> V.Vector Int32
getIntsFromTbl (IntsTbl offs (IntsV intsV) maxId) idx = 
    -- cmp idx to maxId?
    case dataOffAndLen offs idx of
      (Nothing, _) -> V.empty
      (Just off, maybeLen) ->
          V.slice off32 len32 intsV
        where
          off32 = off `div` 4
          len32 = len `div` bytesPerInt
          -- numInts = len `div` bytesPerInt
          len = case maybeLen of
                  Just l -> l
                  Nothing -> (V.length intsV) - off32

firstOffset :: IntsTbl -> Maybe Int
firstOffset (IntsTbl offs _ _) =
    offsetForIdx offs 0

bytesPerInt :: Int
bytesPerInt = 4
