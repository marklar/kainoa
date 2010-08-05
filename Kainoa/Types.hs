module Kainoa.Types where

import qualified Data.ByteString.Lazy as BL

-- entire index
-- data Domain = Domain ResultTbl

data ResultTbl = ResultTbl Pops PopsIdx TextTbl TargetTbl TargetIdxTbl Int

-- ResultTbl columns.
type Pops         = Ints
type PopsIdx      = Ints
{- make PopsTable be a wrapper around BOTH Pops and PopsIdx? -}

type TextTbl      = StrTbl

type TargetTbl    = IntsTbl
type TargetIdxTbl = IntsIdxTbl
{- make the TargetTable be a wrapper around BOTH TargetTbl AND TargetIdxTbl? -}


-- Table types
data StrTbl     = StrTbl     Offsets Strings
data IntsTbl    = IntsTbl    Offsets Ints
data IntsIdxTbl = IntsIdxTbl IntsTbl Int

-- offset files
data Offsets = Offsets BL.ByteString

-- data files
data Strings = Strings BL.ByteString
data Ints    = Ints    BL.ByteString


