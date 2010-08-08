module Kainoa.BoolBL
( openBoolBL
) where

import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (liftM)
import System.Posix.Files (fileExist)

import Kainoa.Types
import Kainoa.Util.ByteString (readInt, substr)
import Kainoa.Util.Integral (toInt, toInt64)


openBoolBL :: FilePath -> FilePath -> IO (Maybe BoolBL)
openBoolBL dir name = do
  pred <- fileExist fileName
  case pred of
    False -> return Nothing
    True  -> do
        boolBL <- liftM BoolBL $ unsafeMMapFile fileName
        return $ Just boolBL
  where
    fileName = dir ++ "/" ++ name


getBool :: BoolBL -> Int -> Bool
getBool (BoolBL bools) id =
    case substr bools offset 1 of  -- bytesPerBool
      Nothing -> False
      Just s -> case readInt s of
                  0 -> False
                  1 -> True
    where
      offset = toInt64 id

-- Store this in BoolBL type?
getNumBools :: BoolBL -> Int
getNumBools (BoolBL bools) =
    toInt $ BL.length bools
