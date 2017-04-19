-- Problem 21 to 28
-- https://wiki.haskell.org/99_questions/21_to_28

module P30
( ctl_insertAt
, ctl_range
) where

import Data.Ix

-- Problem 21
-- Insert an element at a given position into a list.
--
-- Example:
--
-- P21> insertAt 'X' "abcd" 2
-- "aXbcd"
ctl_insertAt :: a -> [a] -> Int -> [a]
ctl_insertAt x xs k =
    let len = length xs; j = max 1 $ min k $ (len + 1) in
    foldl
        (\r (a, n) ->
            let middle = if n == j then [x] else []
                after = if j > len && n == len then [x] else [] in
            r ++ middle ++ [a] ++ after)
        [] $ zip xs $ range (1, len)


-- Problem 22
-- Create a list containing all integers within a given range.
--
-- Example:
-- 
-- Prelude> range 4 9
-- [4,5,6,7,8,9]
ctl_range :: Int -> Int -> [Int]
ctl_range start end
    | start > end   = error "start shouldn't be greater than end"
    | start == end  = [start]
    | otherwise     = ([start]++) $ ctl_range (start + 1) end
