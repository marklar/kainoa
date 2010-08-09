module Kainoa.Types where

import Data.Word (Word8)
import Data.Int (Int32)
import qualified Data.Vector.Storable as V
import Data.List (intercalate)

data Domain = Domain String Lexicon Matrix ResultTbl (Maybe TagTbl)

data Lexicon = Lexicon IntsV StrTbl Int

-- Use these structs for structural typing.
-- Can create a new Matrix by simply using {resIds = foo, popIds = bar}.
data Matrix = Matrix
    { resIds :: IntsTbl
    , popIds :: IntsTbl
    }

data ResultTbl = ResultTbl 
    { pops       :: IntsV
    , popsIdx    :: IntsV
    , texts      :: StrTbl
    , targets    :: IntsTbl
    , targetsIdx :: IntsIdxTbl
    , isFauxV    :: Maybe BoolV
    , numResults :: Int
    }

data TagTbl = TagTbl
    { typeIds   :: IntsV
    , availGlus :: IntsV
    , totalGlus :: IntsV
    , numTags   :: Int
    }

data Tag = Tag
    { tagId    :: Int
    , tagName  :: String
    , tagAvail :: Int
    , tagTotal :: Int
    }

data Result = Result Int Int String (V.Vector Int32)
            deriving (Eq)
instance Show Result where
    show (Result id pop text targetIds) =
        "{" ++ (intercalate ", " strs) ++ "}"
        where strs = [ "id:"         ++ show id
                     , "pop:"        ++ show pop
                     , "text:\""     ++ text ++ "\""
                     , "target_ids:" ++ show targetIds
                     ]

data ResultSet = ResultSet [Result]
instance Show ResultSet where
    show (ResultSet rs) =
        "{success:true, results:[\n" ++
        intercalate ",\n" (map show rs) ++ "\n]}"

-- Basic Tables
data StrTbl     = StrTbl     Offsets (V.Vector Word8) Int {-len-}
data IntsTbl    = IntsTbl    Offsets IntsV            Int {-len-}
data IntsIdxTbl = IntsIdxTbl IntsTbl Int {-firstUsedId-}

-- Simple Vectors
data Offsets = Offsets (V.Vector Int32)
data IntsV   = IntsV   (V.Vector Int32)
data BoolV   = BoolV   (V.Vector Bool)
