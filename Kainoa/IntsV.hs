module Kainoa.IntsV
( openIntsV
, getNumInts
, getInt
) where

import Data.Vector.Storable.MMap (unsafeMMapVector)
import qualified Data.Vector.Storable as V
import Control.Monad (liftM)
import Data.Int (Int32)

import Kainoa.Types
import Kainoa.Util.Integral (toInt)


openIntsV :: FilePath -> FilePath -> IO IntsV
openIntsV dir name = do
  liftM IntsV $ (unsafeMMapVector (dir ++ "/" ++ name) Nothing :: IO (V.Vector Int32))

getInt :: IntsV -> Int -> Maybe Int
getInt (IntsV ints) id =
    Just $ toInt $ ints V.! id

getNumInts :: IntsV -> Int
getNumInts (IntsV ints) =
    V.length ints
