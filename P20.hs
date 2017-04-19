-- Problem 11 to 20
-- https://wiki.haskell.org/99_questions/11_to_20

module P20
(
ctl_encode2
, Encode (..)
, ctl_decode
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
