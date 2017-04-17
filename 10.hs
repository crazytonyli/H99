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
