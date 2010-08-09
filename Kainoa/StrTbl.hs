module Kainoa.StrTbl
( openStrTbl
, getStrFromTbl
, strTblMaxId
) where

import Data.Vector.Storable.MMap (unsafeMMapVector)
import qualified Data.Vector.Storable as V
import Data.Int (Int32)
import Data.Char (chr)
import Data.Word (Word8)
import Control.Monad (liftM)

import Kainoa.Offsets (dataOffAndLen, offsetsMaxId)
import Kainoa.Util.Integral (toInt)
import Kainoa.Types

openStrTbl :: FilePath -> FilePath -> IO StrTbl
openStrTbl dir root = do
  offs <- liftM Offsets $ (unsafeMMapVector (prefix ++ ".offs") Nothing :: IO (V.Vector Int32))
  strs <- unsafeMMapVector (prefix ++ ".data") Nothing :: IO (V.Vector Word8)
  let tbl = StrTbl offs strs (offsetsMaxId offs)
  return tbl
  where
    prefix = dir ++ "/" ++ root

getStrFromTbl :: StrTbl -> Int -> String
getStrFromTbl (StrTbl offs strs maxId) idx =
    -- Use maxId?  (cmp w/ idx)
    case dataOffAndLen offs idx of
      (Nothing, _) -> ""
      (Just off, maybeLen) ->
          {-
            Index contains Latin1, not UTF8.
            When Haskell hears 'Char' it thinks 'UTF8',
            so we cannot treat the data as V.Vector Char.
            Instead, it's V.Vector Word8 (unsigned ints),
            then we convert the Word8s to Chars.
           -}
          stringifyWord8List . V.toList $ word8s
        where
          word8s = V.slice off len strs
          stringifyWord8List = map (chr . toInt)
          len = case maybeLen of
                  Just l -> l
                  Nothing -> (V.length strs) - off

strTblMaxId :: StrTbl -> Int
strTblMaxId (StrTbl _ _ len) = len
