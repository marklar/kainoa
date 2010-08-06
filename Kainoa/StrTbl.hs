module Kainoa.StrTbl
( openStrTbl
, getStrFromTbl
, strTblMaxId
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)

import Kainoa.Offsets (dataOffAndLen)
import Kainoa.Util.ByteString (substr)
import Kainoa.Util.Integral (toInt)
import Kainoa.Types

openStrTbl :: FilePath -> FilePath -> IO StrTbl
openStrTbl dir root = do
  offs <- liftM Offsets $ unsafeMMapFile (prefix ++ ".offs")
  strs <- unsafeMMapFile (prefix ++ ".data")
  return $ StrTbl offs strs (len offs)
  where
    prefix = dir ++ "/" ++ root
    len (Offsets os) = (toInt $ BL.length os) `div` 4

getStrFromTbl :: StrTbl -> Int -> Maybe BL.ByteString
getStrFromTbl (StrTbl offs strs maxId) idx =
    -- Use maxId?  (cmp w/ idx)
    case dataOffAndLen offs idx of
      (Nothing, _) -> Nothing
      (Just off, mLen) -> substr strs off len
          where
            len = case mLen of
                    Just l -> l
                    Nothing -> (BL.length strs) - off

strTblMaxId :: StrTbl -> Int
strTblMaxId (StrTbl _ _ len) = len
