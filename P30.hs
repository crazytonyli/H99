-- Problem 21 to 28
-- https://wiki.haskell.org/99_questions/21_to_28

module P30
( ctl_insertAt
, ctl_range
, ctl_randomSelect
, ctl_diffSelect
) where

import P20
import Data.Ix
import System.Random

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


-- Problem 23
-- Extract a given number of randomly selected elements from a list.
--
-- Example:
--
-- Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
-- eda
ctl_randomSelect :: [a] -> Int -> IO [a]
ctl_randomSelect xs n
    | n == 0 || length xs == 0 = return []
    | otherwise = do
        g <- newStdGen
        (e, left) <- return $ ctl_removeAt (fst $ randomR (1, length xs) g) xs
        r <- ctl_randomSelect left (n - 1)
        return ([e] ++ r)


-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
--
-- Example in Haskell:
--
-- Prelude System.Random>diff_select 6 49
-- Prelude System.Random>[23,1,17,33,21,37]
ctl_diffSelect :: Int -> Int -> IO [Int]
ctl_diffSelect n m = ctl_randomSelect [1..m] n
