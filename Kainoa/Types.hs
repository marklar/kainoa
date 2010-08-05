module Kainoa.Types where

import qualified Data.ByteString.Lazy as BL

-- entire index
data Domain = Domain StrTbl IntsTbl IntsIdxTbl

-- individual tables (columns)
data StrTbl     = StrTbl     Offsets Strings
data IntsTbl    = IntsTbl    Offsets Ints
data IntsIdxTbl = IntsIdxTbl IntsTbl Int

-- offset files
data Offsets = Offsets BL.ByteString

-- data files
data Strings = Strings BL.ByteString
data Ints    = Ints    BL.ByteString


