module ValSpec where

import Test.Hspec
import Val

main :: IO ()
main = hspec $ do
    describe "Val data type" $ do
        it "shows IntVal correctly" $ do
            show (IntVal 42) `shouldBe` "42"
            
        it "shows FloatVal correctly" $ do
            show (FloatVal 3.14) `shouldBe` "3.14"
            
        it "shows StringVal correctly" $ do
            show (StringVal "Hello") `shouldBe` "Hello"
            
        it "shows BoolVal True correctly" $ do
            show (BoolVal True) `shouldBe` "TRUE"
            
        it "shows BoolVal False correctly" $ do
            show (BoolVal False) `shouldBe` "FALSE"
            
        it "shows NilVal correctly" $ do
            show NilVal `shouldBe` "NIL"
    
    describe "valToString function" $ do
        it "converts IntVal to string" $ do
            valToString (IntVal 42) `shouldBe` "42"
            
        it "converts FloatVal to string" $ do
            valToString (FloatVal 3.14) `shouldBe` "3.14"
            
        it "converts StringVal to string" $ do
            valToString (StringVal "Hello") `shouldBe` "Hello"
            
        it "converts BoolVal True to string" $ do
            valToString (BoolVal True) `shouldBe` "TRUE"
            
        it "converts BoolVal False to string" $ do
            valToString (BoolVal False) `shouldBe` "FALSE"
            
        it "converts NilVal to string" $ do
            valToString NilVal `shouldBe` "NIL"