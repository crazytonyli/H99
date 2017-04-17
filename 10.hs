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
ctl_but_last :: [a] -> a
ctl_but_last (x:[_]) = x
ctl_but_last (x:xs) = ctl_but_last xs
ctl_but_last _ = error "Not an empty list"


-- Problem #3
-- Find the K'th element of a list. The first element in the list is number 1.
--
-- Example:
-- Prelude> elementAt [1,2,3] 2
-- 2
-- Prelude> elementAt "haskell" 5
-- 'e'
ctl_elementAt :: Eq a => [a] -> Int -> a
ctl_elementAt [] _ = error "Not an empty list"
ctl_elementAt (x:xs) k
    | k < 1     = error "'k' starts from 1"
    | k == 1    = x
    | xs == []  = error "'k' is out of bounds"
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
