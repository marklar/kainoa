module Kainoa.Types where

import qualified Data.ByteString.Lazy as BL

-- entire index
-- data Domain = Domain ResultTbl

-- data ResultTbl = ResultTbl Pops PopsIdx TextTbl TargetTbl TargetIdxTbl Int
data ResultTbl = ResultTbl 
    { pops       :: Pops
    , popsIdx    :: PopsIdx
    , texts      :: TextTbl
    , targets    :: TargetTbl
    , targetsIdx :: TargetIdxTbl
    , len        :: Int
    }

-- ResultTbl columns.
type Pops         = IntsBL
type PopsIdx      = IntsBL
{- make PopsTable be a wrapper around BOTH Pops and PopsIdx? -}

type TextTbl      = StrTbl

type TargetTbl    = IntsTbl
type TargetIdxTbl = IntsIdxTbl
{- make the TargetTable be a wrapper around BOTH TargetTbl AND TargetIdxTbl? -}


-- Table types
data StrTbl     = StrTbl     Offsets StringsBL
data IntsTbl    = IntsTbl    Offsets IntsBL
data IntsIdxTbl = IntsIdxTbl IntsTbl Int

-- offset files
data Offsets = Offsets BL.ByteString

-- data files
data StringsBL = StringsBL BL.ByteString
data IntsBL    = IntsBL    BL.ByteString


