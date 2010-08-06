module Kainoa.StrTbl
( openStrTbl
, getStrFromTbl
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)

import Kainoa.Offsets (dataOffAndLen)
import Kainoa.Util (substr)
import Kainoa.Types

openStrTbl :: FilePath -> FilePath -> IO StrTbl
openStrTbl dir root =
    do let prefix = dir ++ "/" ++ root
       offs <- liftM Offsets   $ unsafeMMapFile (prefix ++ ".offs")
       strs <- liftM StringsBL $ unsafeMMapFile (prefix ++ ".data")
       return $ StrTbl offs strs

getStrFromTbl :: StrTbl -> Int -> Maybe BL.ByteString
getStrFromTbl (StrTbl offs (StringsBL strs)) idx = 
    case dataOffAndLen offs idx of
      (Nothing, _) -> Nothing
      (Just off, mLen) -> substr strs off len
          where
            len = case mLen of
                    Just l -> l
                    Nothing -> (BL.length strs) - off
