module Kainoa.Types where

import Data.List (intercalate)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy as BL

data Domain = Domain String {-name-} Lexicon Matrix ResultTbl

data Lexicon = Lexicon IntsBL StrTbl Int

data Matrix = Matrix IntsTbl {-ids-} IntsTbl {-pops-}

data ResultTbl = ResultTbl 
    { pops       :: IntsBL
    , popsIdx    :: IntsBL
    , texts      :: StrTbl
    , targets    :: IntsTbl
    , targetsIdx :: IntsIdxTbl
    , len        :: Int
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

-- Tables
data StrTbl     = StrTbl     Offsets BL.ByteString Int {-len-}
data IntsTbl    = IntsTbl    Offsets IntsBL        Int {-len-}
data IntsIdxTbl = IntsIdxTbl IntsTbl Int {-firstUsedId-}

-- Simple "Arrays"
data Offsets = Offsets BL.ByteString
data IntsBL  = IntsBL  BL.ByteString

