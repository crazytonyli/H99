-- Problem 11 to 20
-- https://wiki.haskell.org/99_questions/11_to_20

module P20
(
ctl_encode2
, Encode (..)
, ctl_decode
, ctl_dupli
, ctl_repli
, ctl_dropEvery
, ctl_split
, ctl_slice
, ctl_rotate
, ctl_removeAt
) where

import P10
import Data.Ix

-- Problem 11
-- Modified run-length encoding.
-- 
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.
-- 
-- Example:
-- P11> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']
data Encode a = Single a | Multiple Int a deriving (Show, Eq)
ctl_encode2 :: Eq a => [a] -> [Encode a]
ctl_encode2 xs = map
    (\ps -> let len = length ps; ch = head ps in
            if len  == 1 then Single ch else Multiple len ch)
    $ ctl_pack xs


-- Problem 12
-- Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. Construct
-- its uncompressed version.
--
-- Example:
-- P12> decodeModified
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
ctl_decode :: Eq a => [Encode a] -> [a]
ctl_decode xs = foldl
    (\r x ->
        case x of
            Single e     -> r ++ [e]
            Multiple n e -> r ++ (replicate n e))
    [] xs


-- Problem 13
-- Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in
-- problem 9, but only count them. As in problem P11, simplify the result list
-- by replacing the singleton lists (1 X) by X.
--
-- Example:
--
-- P13> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']


-- Problem 14
-- Duplicate the elements of a list.
--
-- Example in Haskell:
--
-- > dupli [1, 2, 3]
-- [1,1,2,2,3,3]
ctl_dupli :: [a] -> [a]
ctl_dupli = foldl (\r x -> r ++ [x,x]) []


-- Problem 15
-- Replicate the elements of a list a given number of times.
--
-- Example:
--
-- > repli "abc" 3
-- "aaabbbccc"
ctl_repli :: [a] -> Int -> [a]
ctl_repli xs n = foldl (\r x -> r ++ (replicate n x)) [] xs


-- Problem 16
-- Drop every N'th element from a list.
--
-- Example:
--
-- *Main> dropEvery "abcdefghik" 3
-- "abdeghk"
ctl_dropEvery :: [a] -> Int -> [a]
ctl_dropEvery xs n = foldl
    (\r (x, i) -> if n <= 0 || i `mod` n == 0 then r else r ++ [x])
    [] $ zip xs $ range (1, length xs)


-- Problem 17
-- Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- Example:
--
-- *Main> split "abcdefghik" 3
-- ("abc", "defghik")
ctl_split :: [a] -> Int -> ([a], [a])
ctl_split xs n = foldl
    (\(a, b) (x, i) -> if i <= n then (a ++ [x], b) else (a, b ++ [x]))
    ([], []) $ zip xs $ range (1, length xs)


-- Problem 18
-- Extract a slice from a list.
--
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.
--
-- Example:
--
-- *Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
ctl_slice :: [a] -> Int -> Int -> [a]
ctl_slice xs start end = foldl
    (\r (x, n) -> if n >= start && n <= end then r ++ [x] else r)
    [] $ zip xs $ range (1, length xs)


-- Problem 19
-- Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- Examples in Haskell:
--
-- *Main> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
--
-- *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
ctl_rotate :: [a] -> Int -> [a]
ctl_rotate xs n =
    b ++ a
    where len = length xs
          md = n `mod` len
          m = if md < 0 then md + len else md
          (a,b) = ctl_split xs m


-- Problem 20
-- Remove the K'th element from a list.
--
-- Example in Haskell:
--
-- *Main> removeAt 2 "abcd"
-- ('b',"acd")
ctl_removeAt :: Int -> [a] -> (a, [a])
ctl_removeAt n xs
    | n <= 0 || n >= length xs = error "out of bounds"
    | otherwise = foldl
        (\(a, as) (x, i) -> if i == n then (x, as) else (a, as ++ [x]))
        ((head xs), []) $ zip xs $ range (1, length xs)
