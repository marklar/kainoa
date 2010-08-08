module Kainoa.Types where

import Data.List (intercalate)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy as BL

data Domain = Domain String Lexicon Matrix ResultTbl (Maybe TagTbl)

data Lexicon = Lexicon IntsBL StrTbl Int

-- Use these structs for structural typing.
-- Can create a new Matrix by simply using {resIds = foo, popIds = bar}.
data Matrix = Matrix
    { resIds :: IntsTbl
    , popIds :: IntsTbl
    }

data ResultTbl = ResultTbl 
    { pops       :: IntsBL
    , popsIdx    :: IntsBL
    , texts      :: StrTbl
    , targets    :: IntsTbl
    , targetsIdx :: IntsIdxTbl
    , isFauxBL   :: Maybe BoolBL
    , numResults :: Int
    }

data TagTbl = TagTbl
    { typeIds   :: IntsBL
    , availGlus :: IntsBL
    , totalGlus :: IntsBL
    , numTags   :: Int
    }

data Tag = Tag
    { tagId    :: Int
    , tagName  :: String
    , tagAvail :: Int
    , tagTotal :: Int
    }

data Result = Result Int Int BL.ByteString [Int]
            deriving (Eq)
instance Show Result where
    show (Result id pop text targetIds) =
        "{" ++ (intercalate ", " strs) ++ "}"
        where strs = [ "id:"         ++ show id
                     , "pop:"        ++ show pop
                     , "text:\""     ++ unpack text ++ "\""
                     , "target_ids:" ++ show targetIds
                     ]

data ResultSet = ResultSet [Result]
instance Show ResultSet where
    show (ResultSet rs) =
        "{success:true, results:\n" ++ intercalate ",\n" (map show rs) ++ "}"

-- Basic Tables
data StrTbl     = StrTbl     Offsets BL.ByteString Int {-len-}
data IntsTbl    = IntsTbl    Offsets IntsBL        Int {-len-}
data IntsIdxTbl = IntsIdxTbl IntsTbl Int {-firstUsedId-}

-- Simple "Arrays"
data Offsets = Offsets BL.ByteString
data IntsBL  = IntsBL  BL.ByteString
data BoolBL  = BoolBL  BL.ByteString
