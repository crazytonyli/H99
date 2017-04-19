import P10
import P20
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Problem #1" $ do
        it "finds the last element of a list" $ do
            ctl_last [] `shouldThrow` anyErrorCall
            ctl_last [1,2,3] `shouldBe` 3
            ctl_last "abcde" `shouldBe` 'e'

    describe "Problem #2" $ do
        it "finds the last but one element of a list" $ do
            evaluate (ctl_butLast []) `shouldThrow` anyErrorCall
            evaluate (ctl_butLast [1]) `shouldThrow` anyErrorCall
            ctl_butLast [1,2] `shouldBe` 1
            ctl_butLast [1,2,3] `shouldBe` 2
            ctl_butLast ['a'..'z'] `shouldBe` 'y'

    describe "Problem #3" $ do
        it "finds the K'th element of a list" $ do
            evaluate (ctl_elementAt ([] :: [Int]) 2) `shouldThrow` anyErrorCall
            evaluate (ctl_elementAt [1] 2) `shouldThrow` anyErrorCall
            ctl_elementAt [1,2] 2 `shouldBe` 2
            ctl_elementAt [1,2,3] 1 `shouldBe` 1
            ctl_elementAt [1,2,3] 2 `shouldBe` 2
            ctl_elementAt [1,2,3] 3 `shouldBe` 3

    describe "Problem #4" $ do
        it "finds the number of elements of a list" $ do
            ctl_length [] `shouldBe` 0
            ctl_length [1] `shouldBe` 1
            ctl_length [1,2] `shouldBe` 2
            ctl_length [1,2,3] `shouldBe` 3

    describe "Problem #5" $ do
        it "reverses a list" $ do
            ctl_reverse ([] :: [Int]) `shouldBe` []
            ctl_reverse [1] `shouldBe` [1]
            ctl_reverse [1,2] `shouldBe` [2,1]
            ctl_reverse [1,2,3] `shouldBe` [3,2,1]

    describe "Problem #6" $ do
        it "finds out whether a list is a palindrome" $ do
            ctl_isPalindrome "hello" `shouldBe` False
            ctl_isPalindrome "mom" `shouldBe` True

    describe "Problem #7" $ do
        it "flattens a nested list structure" $ do
            ctl_flatten (Elem 5) `shouldBe` [5]
            ctl_flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1,2,3,4,5]

    describe "Problem #8" $ do
        it "eliminates consecutive duplicates of list elements." $ do
            ctl_compress "" `shouldBe` ""
            ctl_compress "abcd" `shouldBe` "abcd"
            ctl_compress "aaaabccaadeeee" `shouldBe` "abcade"

    describe "Problem #9" $ do
        it "packs into sublists" $ do
            ctl_pack "abbbccdee" `shouldBe` ["a","bbb","cc","d","ee"]
            ctl_pack "aabbcddee" `shouldBe` ["aa","bb","c","dd","ee"]

    describe "Problem #10" $ do
        it "encodes duplicates in a list" $ do
            ctl_encode "aaaabccaadeeee" `shouldBe` [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

    describe "Problem #11" $ do
        it "encodes duplicated in a list - 2" $ do
            ctl_encode2 "abbcccdddd" `shouldBe` [Single 'a', (Multiple 2 'b'), (Multiple 3 'c'), (Multiple 4 'd')]

    describe "Problem #12" $ do
        it "decodes a run-length encoded list" $ do
            ctl_decode [Single 'a', (Multiple 2 'b'), (Multiple 3 'c'), (Multiple 4 'd')] `shouldBe` "abbcccdddd"

    describe "Problem #14" $ do
        it "duplicates the elements of a list" $ do
            ctl_dupli ([] :: [Int]) `shouldBe` []
            ctl_dupli [1] `shouldBe` [1,1]
            ctl_dupli [1,2,3] `shouldBe` [1,1,2,2,3,3]

    describe "Problem #15" $ do
        it "replicates the elements of a list" $ do
            ctl_repli "abc" 0 `shouldBe` ""
            ctl_repli "abc" 1 `shouldBe` "abc"
            ctl_repli "abc" 2 `shouldBe` "aabbcc"
            ctl_repli "abc" 3 `shouldBe` "aaabbbccc"

    describe "Problem #16" $ do
        it "drops every N'th element from a list" $ do
            ctl_dropEvery "abcdefg" 0 `shouldBe` ""
            ctl_dropEvery "abcdefg" 1 `shouldBe` ""
            ctl_dropEvery "abcdefg" 2 `shouldBe` "aceg"
            ctl_dropEvery "abcdefg" 3 `shouldBe` "abdeg"
            ctl_dropEvery "abcdefg" 4 `shouldBe` "abcefg"
            ctl_dropEvery "abcdefg" 10 `shouldBe` "abcdefg"

    describe "Problem #17" $ do
        it "splits a list into two parts" $ do
            ctl_split "" 3 `shouldBe` ("", "")
            ctl_split "ab" 3 `shouldBe` ("ab", "")
            ctl_split "abc" 3 `shouldBe` ("abc", "")
            ctl_split "abcde" 3 `shouldBe` ("abc", "de")

    describe "Problem #18" $ do
        it "extracts a slice from a list" $ do
            ctl_slice "abcdefg" 2 1 `shouldBe` ""
            ctl_slice "abcdefg" 1 3 `shouldBe` "abc"
            ctl_slice "abcdefg" 2 4 `shouldBe` "bcd"

    describe "Problem #19" $ do
        it "rotates a list N places to the left" $ do
            ctl_rotate "abcdefgh" 3 `shouldBe` "defghabc"
            ctl_rotate "abcdefgh" 11 `shouldBe` "defghabc"
            ctl_rotate "abcdefgh" (-5) `shouldBe` "defghabc"
            ctl_rotate "abcdefgh" (-13) `shouldBe` "defghabc"

    describe "Problem #20" $ do
        it "removes the K'th element from a list" $ do
            ctl_removeAt 2 "abcde" `shouldBe` ('b', "acde")
            evaluate (ctl_removeAt 0 "abcde") `shouldThrow` anyErrorCall
            evaluate (ctl_removeAt 10 "abcde") `shouldThrow` anyErrorCall
