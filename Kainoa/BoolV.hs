module Kainoa.BoolV
( openBoolV
, getBool
) where

import Data.Vector.Storable.MMap (unsafeMMapVector)
import qualified Data.Vector.Storable as V
import Control.Monad (liftM)
import System.Posix.Files (fileExist)

import Kainoa.Types

openBoolV :: FilePath -> FilePath -> IO (Maybe BoolV)
openBoolV dir name = do
  pred <- fileExist fileName
  case pred of
    False -> return Nothing
    True  -> do
        boolV <- liftM BoolV $
                 ((unsafeMMapVector fileName Nothing) :: IO (V.Vector Bool))
        return $ Just boolV
  where
    fileName = dir ++ "/" ++ name

getBool :: BoolV -> Int -> Bool
getBool (BoolV bools) id =
    bools V.! id
