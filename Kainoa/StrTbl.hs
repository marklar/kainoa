module Kainoa.StrTbl
( makeStrTbl
, getStrFromTbl
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)

import Kainoa.Offsets (dataOffAndLen, makeOffsets)
import Kainoa.Util (substr)
import Kainoa.Types

makeStrTbl :: FilePath -> FilePath -> IO StrTbl
makeStrTbl dir root =
    do let prefix = dir ++ "/" ++ root
       offs <- liftM makeOffsets $ unsafeMMapFile (prefix ++ ".offs")
       strs <- liftM Strings     $ unsafeMMapFile (prefix ++ ".data")
       return $ StrTbl offs strs

getStrFromTbl :: StrTbl -> Int -> Maybe BL.ByteString
getStrFromTbl (StrTbl offs (Strings strs)) idx = 
    case dataOffAndLen offs idx of
      (Nothing, _) -> Nothing
      (Just off, mLen) -> substr strs off len
          where
            len = case mLen of
                    Just l -> l
                    Nothing -> (BL.length strs) - off
