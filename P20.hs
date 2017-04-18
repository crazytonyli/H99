-- Problem 11 to 20
-- https://wiki.haskell.org/99_questions/11_to_20

module P20
(
ctl_encode2
, Encode (..)
) where

import P10

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
