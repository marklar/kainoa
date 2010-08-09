module Kainoa.Offsets
( dataOffAndLen
, offsetForIdx
, offsetsMaxId
) where

import Data.Vector.Storable.MMap (unsafeMMapVector)
import qualified Data.Vector.Storable as V

import Kainoa.Util.Integral (toInt)
import Kainoa.Types

offsetsMaxId :: Offsets -> Int
offsetsMaxId (Offsets v) = V.length v

dataOffAndLen :: Offsets -> Int -> (Maybe Int, Maybe Int)
dataOffAndLen offs idx =
    case offsetForIdx offs idx of
      Nothing -> (Nothing, Nothing)
      Just off ->
          (Just off, len)
        where
          len = case offsetForIdx offs (idx+1) of
                  Nothing -> Nothing
                  Just o -> Just $ o - off

offsetForIdx :: Offsets -> Int -> Maybe Int
offsetForIdx (Offsets offs) idx =
    if idx > V.length offs then
        Nothing
    else
        Just $ toInt $ offs V.! (idx-1)
