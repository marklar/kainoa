module Kainoa.Offsets
( dataOffAndLen
, offsetForIdx
, offsetsMaxId
) where

import qualified Data.ByteString.Lazy as BL

import Kainoa.Util.ByteString (readInt64, substr)
import Kainoa.Util.Integral (toInt, toInt64, Int64)
import Kainoa.Types

offsetsMaxId :: Offsets -> Int
offsetsMaxId (Offsets bl) =
    toInt (BL.length bl) `div` 4

dataOffAndLen :: Offsets -> Int -> (Maybe Int64, Maybe Int64)
dataOffAndLen offs idx =
    case offsetForIdx offs idx of
      Nothing -> (Nothing, Nothing)
      Just off -> (Just off, len)
        where
          len = case offsetForIdx offs (idx+1) of
                  Nothing -> Nothing
                  Just o -> Just (o - off)

offsetForIdx :: Offsets -> Int -> Maybe Int64
offsetForIdx (Offsets offs) idx =
    case substr offs offsOff 4 of
      Just s -> Just (readInt64 s)
      Nothing -> Nothing
    where 
      offsOff = toInt64 $ (idx-1) * 4

