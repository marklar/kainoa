module Kainoa.Util.ByteString
( readInt
, readInt64
, readInts
, substr
) where

import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

import Kainoa.Util.Integral (toInt, toInt64)

readInt :: BL.ByteString -> Int
readInt = toInt . readInt64

-- Take first 4 bytes (32 bits) of bl;
-- convert into Int64.
-- What if there aren't 4 bytes?
readInt64 :: BL.ByteString -> Int64
readInt64 bl =
    toInt64 $ runGet getWord32host bl

readInts :: BL.ByteString -> Int64 -> Int -> [Int]
readInts bl start num =
    takeWhile (/= (-1)) $ map readInt [0 .. toInt64 (num-1)]
    where readInt i = case substr bl (start + (i*4)) 4 of
                        Nothing -> -1
                        Just bl -> toInt $ readInt64 bl

-- Grab substring of bl.
-- If 'start' is out of range -> Nothing.
-- If 'len' is too long, shorten.
substr :: BL.ByteString -> Int64 -> Int64 -> Maybe BL.ByteString
substr bl start len =
    if outOfRange
    then Nothing
    else Just (BL.drop start $ BL.take limit bl)
    where
      outOfRange = start < 0 || start >= BL.length bl
      limit = start+len
