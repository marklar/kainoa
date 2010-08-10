module Kainoa.Result
( cmpId
, cmpPop
) where

import Kainoa.Types
  
cmpId  :: Result -> Result -> Ordering
cmpId (Result i0 _ _ _) (Result i1 _ _ _)
    | i0 == i1   =  EQ
    | i0 <= i1   =  LT
    | otherwise  =  GT    

cmpPop :: Result -> Result -> Ordering
cmpPop (Result _ p0 _ _) (Result _ p1 _ _)
    | p0 == p1   =  EQ
    | p0 <= p1   =  LT
    | otherwise  =  GT    
