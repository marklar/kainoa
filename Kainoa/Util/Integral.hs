module Kainoa.Util.Integral
( toInt
, toInt64
, Int64
) where

import Data.Int (Int64)

toInt64 :: Integral a => a -> Int64
toInt64 i = (fromIntegral i) :: Int64

toInt :: Integral a => a -> Int
toInt i = (fromIntegral i) :: Int
