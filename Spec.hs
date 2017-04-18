import P10
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
