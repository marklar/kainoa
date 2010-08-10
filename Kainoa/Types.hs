module Kainoa.Types where

import Data.Word (Word8)
import Data.Int (Int32)
import qualified Data.Vector.Storable as V
import Data.List (intercalate)

data LexemeInfo = Lexeme String
                | LexemeIds [Int]
                  deriving (Show)

data Query = And  Query Query
           | Or   Query Query
           | Not  Query
           | Leaf LexemeInfo
           | EmptyQuery
             deriving (Show)

data Domain = Domain String Lexicon Matrix ResultTbl (Maybe TagTbl)

data Lexicon = Lexicon IntsV StrTbl Int

data Matrix = Matrix
              IntsTbl -- resIds
              IntsTbl -- popIds

data ResultTbl = ResultTbl 
                 IntsV         -- pop
                 IntsV         -- popIdx
                 StrTbl        -- text
                 IntsTbl       -- targets
                 IntsIdxTbl    -- targetIdx
                 (Maybe BoolV) -- isFaux
                 Int           -- numResults

data TagTbl = TagTbl
              IntsV -- typeIds
              IntsV -- availGlus
              IntsV -- totalGlus
              Int   -- numTags

data Tag = Tag
           Int    -- id
           String -- name
           Int    -- avail
           Int    -- total

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
