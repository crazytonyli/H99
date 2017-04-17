import P10
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Problem #1" $ do
        it "finds the last element of a list" $ do
            ctl_last [] `shouldThrow` anyErrorCall
            ctl_last [1,2,3] `shouldBe` 3
            ctl_last "abcde" `shouldBe` 'e'

    describe "Problem #2" $ do
        it "finds the last but one element of a list" $ do
            ctl_butLast [] `shouldThrow` anyErrorCall
            -- ctl_butLast [1] `shouldThrow` anyErrorCall
            ctl_butLast [1,2] `shouldBe` 1
            ctl_butLast [1,2,3] `shouldBe` 2
            ctl_butLast ['a'..'z'] `shouldBe` 'y'
