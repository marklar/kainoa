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

{- |

A Domain is a set of data structures which together constitute a
single search index.

-}
data Domain = Domain String Lexicon Matrix ResultTbl (Maybe TagTbl)


{- |

The Lexicon stores all words which appear in indexed items.  At search
time, we hunt for the query lexemes in the Lexicon, obtaining for each
a corresponding set of lexical IDs.  (If the query lexeme is treated
as a prefix, we want all indexed lexemes that match.)

The Lexicon is a composed, on-disk structure.  It's a hash-table:
    string-initial character -> ordered set of Strings

Once we find the proper "bucket", we track down the set of all
matching lexemes via binary search.

-}
data Lexicon = Lexicon IntsV StrTbl Int


{- |

The Matrix is a map:
    lexeme ID -> ([Result ID], [Result Popularity Rank])

Given the IDs of the query lexemes, we can look up their correponding
sets of Result ids.

For each lexeme, result IDs are stored twice, in both cases in
pre-sorted order.

  1. Each result is identified by its lexical rank.
  2. Each result is identified by its popularity rank.

This is one of the most important optimizations of the system.  It
allows us to avoid sorting at query time.  At query time, we merely
combine (via intersection or union) ordered sets of integers, which is
a very fast, O(n) operation.

-}
data Matrix = Matrix
IntsTbl -- resIds IntsTbl -- popIds

{- |

The ResultTbl stores all the indexed items.  When we choose to display
an item as a search Result, we can fetch all or merely some subset of
its data from the ResultTbl.  (Since our data store is columnar, not
only is fetching extremely fast, but we pay what little lookup time
there is for only those columns we decide to access.)

For each Result, we store:

  * its data for display (i.e. its text), and
  * its metadata (e.g. its popular rank, its associated "target IDs", etc.).

(A "target" is a corresponding SKU in the MySQL database.)

-}
data ResultTbl = ResultTbl 
                 IntsV         -- pop
                 IntsV         -- popIdx
                 StrTbl        -- text
                 IntsTbl       -- targets
                 IntsIdxTbl    -- targetIdx
                 (Maybe BoolV) -- isFaux
                 Int           -- numResults

{- |
Tags are references into the master MySQL DB, indicating
correspondences between a search result and different products and
product categories.
-}
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

{- |

A Result is corresponds to whatever text (String) was indexed for some
real-world item.  Usually, it's the name of a product.

Each Result can be fetched in constant time (i.e. O(1)) by either its
ID or its Popularity Rank (explained below).

### Lexical Rank (ID)

The Result has an ID, which corresponds to its lexical rank.  The
shorter the text, the better its rank.  There are two reasons for
this.

First, if two different Results are both matches for some query, and
one of those Results has a shorter text against which to match, then
it is probably a closer lexical match than the longer one.

Second, one must be able to find any Result by searching.  If we
select whichever matching Result has the shortest text, we'll always
make it possible to find it (by, at worst, typing the entire Result
text as the query).

### Popularity Rank

The Result also has a popularity rank.  (We re-index very frequently,
to make sure that these ranks accurately reflect reality.)

-}
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

{-
  
## Basic Tables

Tables are used to access data stored across multiple Vectors.  Unlike
Vectors, the records in Tables are indexed from 1.

There are Simple Tables, with just a single column (i.e. data type),
and Complex Tables (which we see above, the parts of a Domain).


### Simple Tables (Generic)

Simple Tables are a single column -- i.e. contain a single data type.

Like the Vectors, the simple Tables are highly reusable for other
programs.

-}

data StrTbl     = StrTbl     Offsets (V.Vector Word8) Int {-len-}
data IntsTbl    = IntsTbl    Offsets IntsV            Int {-len-}
data IntsIdxTbl = IntsIdxTbl IntsTbl Int {-firstUsedId-}



{-

## Simple Vectors (mmap-ed data)

The lowest-level data types are those which wrap the data files.
They are all Vectors of some simple type.
Vectors are always indexed from 0.

-}

data Offsets = Offsets (V.Vector Int32)
data IntsV   = IntsV   (V.Vector Int32)
data BoolV   = BoolV   (V.Vector Bool)
