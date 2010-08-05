module Kainoa.Offsets
( dataOffAndLen
, makeOffsets
, offsetForIdx
) where

import qualified Data.ByteString.Lazy as BL
import Data.Int

import Kainoa.Util (getInt64, substr, toInt64)
import Kainoa.Types

makeOffsets :: BL.ByteString -> Offsets
makeOffsets bs = Offsets bs

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
      Just s -> Just (getInt64 s)
      Nothing -> Nothing
    where 
      offsOff = toInt64 $ (idx-1) * 4

