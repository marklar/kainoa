module Kainoa.Result
( cmpId
, cmpPop
, cmpRes
, getId
, getPop
) where

import Kainoa.Types

cmpRes :: Ord a => (Result -> a) -> Result -> Result -> Ordering
cmpRes getter r0 r1 =
    compare (getter r0) (getter r1)

getId :: Result -> Int
getId (Result id _ _ _) = id

getPop :: Result -> Int
getPop (Result _ p _ _) = p

-----
  
cmpId :: Result -> Result -> Ordering
cmpId (Result i0 _ _ _) (Result i1 _ _ _)
    | i0 == i1   =  EQ
    | i0 <= i1   =  LT
    | otherwise  =  GT    

cmpPop :: Result -> Result -> Ordering
cmpPop (Result _ p0 _ _) (Result _ p1 _ _)
    | p0 == p1   =  EQ
    | p0 <= p1   =  LT
    | otherwise  =  GT    
