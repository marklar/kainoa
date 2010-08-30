module Kainoa.Util.OrdList
( ordNubBy
, ordNub
, ordMergeBy
, ordMerge
, ordMergeNubBy
, ordIntersectNubBy
, ordMergeNub
, ordIntersectNub
, ordDiff
, ordDiffBy
) where


{-
  Eliminates duplicate entries from the list, where duplication is defined
  by the 'eq' function.  Keeps first value.
-}
ordNubBy :: (a -> a -> Bool) -> [a] -> [a]
ordNubBy eq (x1 : xs@(x2 : _)) =
   if eq x1 x2 then ordNubBy eq xs else x1 : ordNubBy eq xs
ordNubBy _ xs = xs
 
ordNub :: (Eq a) => [a] -> [a]
ordNub = ordNubBy (==)

-----

{- 
   Merge two ord lists into a new ord list.  Where elements are equal
   the element from the first list is taken first.
-}
ordMergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
ordMergeBy _ [] ys = ys
ordMergeBy _ xs [] = xs
ordMergeBy cmp xs@(x1:xs1) ys@(y1:ys1) =
   if cmp x1 y1 == GT
      then y1 : ordMergeBy cmp xs ys1
      else x1 : ordMergeBy cmp xs1 ys
 
ordMerge :: (Ord a) => [a] -> [a] -> [a]
ordMerge = ordMergeBy compare

ordMergeNub :: (Eq a, Ord a) => [a] -> [a] -> [a]
ordMergeNub = ordMergeNubBy compare  -- more efficient?
-- ordMergeNub xs ys = ordNub $ ordMerge xs ys   -- less efficient?

ordMergeNubBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
ordMergeNubBy _ [] ys = ys
ordMergeNubBy _ xs [] = xs
ordMergeNubBy cmp xs@(x:xs') ys@(y:ys') =
   case cmp x y of
     LT -> x : ordMergeNubBy cmp xs' ys
     EQ -> x : ordMergeNubBy cmp xs' ys' 
     GT -> y : ordMergeNubBy cmp xs  ys'

-----

ordIntersectNub :: (Eq a, Ord a) => [a] -> [a] -> [a]
ordIntersectNub = ordIntersectNubBy compare

ordIntersectNubBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
ordIntersectNubBy _ [] _ = []
ordIntersectNubBy _ _ [] = []
ordIntersectNubBy cmp xs@(x:xs') ys@(y:ys') =
    case cmp x y of
      LT ->     ordIntersectNubBy cmp xs' ys
      GT ->     ordIntersectNubBy cmp xs  ys'
      EQ -> x : ordIntersectNubBy cmp xs' ys'

-----

ordDiff :: (Eq a, Ord a) => [a] -> [a] -> [a]
ordDiff = ordDiffBy compare

ordDiffBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
ordDiffBy _ [] _  = []
ordDiffBy _ xs [] = xs
ordDiffBy cmp xs@(x:xs') ys@(y:ys') =
    case cmp x y of
      LT -> x : ordDiffBy cmp xs' ys
      GT ->     ordDiffBy cmp xs  ys'
      EQ ->     ordDiffBy cmp xs' ys'
