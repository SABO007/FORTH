module InterpretSpec where

import Test.Hspec
import Interpret
import Val

main :: IO ()
main = hspec $ do
    describe "interpret function" $ do
        it "interprets basic arithmetic" $ do
            let (stack, _) = interpret "5 10 +"
            stack `shouldBe` [IntVal 15]
            
        it "interprets multiple operations" $ do
            let (stack, _) = interpret "5 10 + 2 *"
            stack `shouldBe` [IntVal 30]
            
        it "interprets string operations" $ do
            let (stack, _) = interpret "\"Hello\" \"World\" CONCAT2"
            stack `shouldBe` [StringVal "HelloWorld"]
            
        it "handles empty input" $ do
            let (stack, _) = interpret ""
            stack `shouldBe` []
            
        it "interprets stack manipulation" $ do
            let (stack, _) = interpret "1 2 3 ROT"
            stack `shouldBe` [IntVal 1, IntVal 3, IntVal 2]