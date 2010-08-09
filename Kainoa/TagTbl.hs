module Kainoa.TagTbl
( openTagTbl
, fetchTag
) where

import qualified Data.Map as Map
import Control.Monad (liftM)
import System.Posix.Files (fileExist)

import Kainoa.Types
import Kainoa.Util.Integral (inRange)
import Kainoa.IntsV (openIntsV, getNumInts, getInt)

openTagTbl :: FilePath -> IO (Maybe TagTbl)
openTagTbl dir = do
  pred <- filesExist dir
  case pred of
    False -> return Nothing
    True  -> do
        typeIds   <- openIntsV dir "tag.type_ids.data"
        availGlus <- openIntsV dir "tag.avail_glus.data"  -- Will mutate!
        totalGlus <- openIntsV dir "tag.total_glus.data"
        return $ Just $ TagTbl typeIds availGlus totalGlus (getNumInts typeIds)

fetchTag :: TagTbl -> Int -> Maybe Tag
fetchTag tagTbl@(TagTbl typeIds availGlus totalGlus len) tagId =
    if inRange (1,len) tagId then
        Just $ Tag tagId typeName numAvail numTotal
    else
        Nothing
    where
      typeName = maybe "foo" id (getTagName typeId)
      typeId   = get typeIds
      numAvail = get availGlus
      numTotal = get totalGlus
      get tbl = maybe 0 id (getInt tbl (tagId-1))

-- not exported --

filesExist :: FilePath -> IO Bool
filesExist dir = do
  -- There MUST be a prettier way to express this!
  liftM (all id) $ mapM fileExist fileNames
      where
        fileNames = map fullName ["type_ids", "avail_glus", "total_glus"]
        fullName s = dir ++ "/tag." ++ s ++ ".data"

getTagName :: Int -> Maybe String
getTagName typeId =
    Map.lookup typeId tagTypeId2Name

-- FixMe: Fetch this data from DB.
tagTypeId2Name :: Map.Map Int String
tagTypeId2Name =
    Map.fromList $ zip [1..20] (cycle ["foo", "bar", "baz"])
