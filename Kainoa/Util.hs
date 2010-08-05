module Kainoa.Util
( getInt
, getInt64
, substr
, getInts
, toInt64
, toInt
) where

import Data.Int
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

{-
  Functions on ByteStrings.
-}

getInt :: BL.ByteString -> Int
getInt = toInt . getInt64

-- Take first 4 bytes (32 bits) of bl;
-- convert into Int64.
-- What if there aren't 4 bytes?
getInt64 :: BL.ByteString -> Int64
getInt64 bl =
    toInt64 $ runGet getWord32host bl

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

getInts :: BL.ByteString -> Int64 -> Int -> [Int]
getInts bl start num =
    takeWhile (/= (-1)) $ map getInt [0 .. toInt64 (num-1)]
    where getInt i = case substr bl (start + (i*4)) 4 of
                       Nothing -> -1
                       Just bl -> toInt $ getInt64 bl
                       -- Just bl -> getInt bl

{-
  Utility functions on Integrals.
-}
toInt64 :: Integral a => a -> Int64
toInt64 i = (fromIntegral i) :: Int64

toInt :: Integral a => a -> Int
toInt i = (fromIntegral i) :: Int
