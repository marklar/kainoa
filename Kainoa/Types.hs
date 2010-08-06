module Kainoa.Types where

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

-- Tables
data StrTbl     = StrTbl     Offsets BL.ByteString Int {-len-}
data IntsTbl    = IntsTbl    Offsets IntsBL        Int {-len-}
data IntsIdxTbl = IntsIdxTbl IntsTbl Int {-firstUsedId-}

-- Simple "Arrays"
data Offsets = Offsets BL.ByteString
data IntsBL  = IntsBL  BL.ByteString

