module P10
( ctl_last
, ctl_butLast
, ctl_elementAt
, ctl_length
, ctl_reverse
, ctl_isPalindrome
, ctl_flatten
, NestedList (..)
, ctl_compress
, ctl_pack
, ctl_encode
) where

-- Problem 1 to 10
-- https://wiki.haskell.org/99_questions/1_to_10


-- Problem #1
-- Find the last element of a list.
--
-- Example:
-- Prelude> myLast [1,2,3,4]
-- 4
-- Prelude> myLast ['x','y','z']
-- 'z'
ctl_last :: [a] -> a
ctl_last [x] = x
ctl_last (_:xs) = ctl_last xs
ctl_last _ = error "Not an empty list"


-- Problem #2
-- Find the last but one element of a list.
--
-- Example:
-- Prelude> myButLast [1,2,3,4]
-- 3
-- Prelude> myButLast ['a'..'z']
-- 'y'
ctl_butLast :: [a] -> a
ctl_butLast [] = error "Not an empty list"
ctl_butLast [_] = error "Not enough elements in list"
ctl_butLast [x,_] = x
ctl_butLast (x:xs) = ctl_butLast xs


-- Problem #3
-- Find the K'th element of a list. The first element in the list is number 1.
--
-- Example:
-- Prelude> elementAt [1,2,3] 2
-- 2
-- Prelude> elementAt "haskell" 5
-- 'e'
ctl_elementAt :: [a] -> Int -> a
ctl_elementAt [] _ = error "Not an empty list"
ctl_elementAt (x:xs) k
    | k < 1     = error "'k' starts from 1"
    | k == 1    = x
    | null xs   = error "'k' is out of bounds"
    | otherwise = ctl_elementAt xs (k - 1)


-- Problem #4
-- Find the number of elements of a list.
--
-- Example:
-- Prelude> myLength [123, 456, 789]
-- 3
-- Prelude> myLength "Hello, world!"
-- 13
ctl_length :: [a] -> Int
ctl_length [] = 0
ctl_length (_:xs) = ctl_length xs + 1


-- Problem 5
-- Reverse a list.
--
-- Example:
-- Prelude> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- Prelude> myReverse [1,2,3,4]
-- [4,3,2,1]
ctl_reverse :: [a] -> [a]
ctl_reverse [] = []
ctl_reverse [x] = [x]
ctl_reverse (x:xs) = ctl_reverse xs ++ [x]


-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or
-- backward; e.g. (x a m a x).
--
-- Example:
-- *Main> isPalindrome [1,2,3]
-- False
-- *Main> isPalindrome "madamimadam"
-- True
-- *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True
ctl_isPalindrome :: Eq a => [a] -> Bool
ctl_isPalindrome xs = xs == (ctl_reverse xs)


-- Problem 7
-- Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements into a `flat' list by
-- replacing each list with its elements (recursively).
--
-- Example:
-- We have to define a new data type, because lists in Haskell are homogeneous.
--
--  data NestedList a = Elem a | List [NestedList a]
-- *Main> flatten (Elem 5)
-- [5]
-- *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- *Main> flatten (List [])
-- []
data NestedList a = Elem a | List [NestedList a]
ctl_flatten :: NestedList a -> [a]
ctl_flatten (Elem x) = [x]
ctl_flatten (List xs) = foldl (\r x -> r ++ ctl_flatten x) [] xs


-- Problem 8
-- Eliminate consecutive duplicates of list elements.
--
-- If a list contains repeated elements they should be replaced with a single
-- copy of the element. The order of the elements should not be changed.
--
-- Example:
-- > compress "aaaabccaadeeee"
-- "abcade"
ctl_compress :: Eq a => [a] -> [a]
ctl_compress xs = foldl
    (\r x ->
        case r of
            [] -> [x]
            _  -> if last r == x then r else r ++ [x])
    [] xs


-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements they should be placed in separate sublists.
--
-- Example:
-- *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
--             'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
ctl_pack :: Eq a => [a] -> [[a]]
ctl_pack xs = foldl
    (\r x ->
        let lr = last r in
        case lr of
            [] -> [[x]]
            _  -> if head lr == x
                  then take (length r - 1) r ++ [lr ++ [x]]
                  else r ++ [[x]])
    [[]] xs


-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.
--
-- Example:
-- encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
ctl_encode :: Eq a => [a] -> [(Int, a)]
ctl_encode xs = map (\ps -> (length ps, head ps)) (ctl_pack xs)
