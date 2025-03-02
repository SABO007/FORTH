module EvalSpec where

import Test.Hspec
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)

import Val
import Eval

-- Helper function to run a sequence of FORTH tokens
runForth :: [String] -> IO [Val]
runForth tokens = do
    let initialEnv = Map.empty
        initialStack = []
    (_, (_, finalStack)) <- runStateT (mapM_ evalForth tokens) (initialEnv, initialStack)
    return finalStack

main :: IO ()
main = hspec $ do
    describe "Basic FORTH operations" $ do
        it "pushes integers onto stack" $ do
            stack <- runForth ["5", "10"]
            stack `shouldBe` [IntVal 10, IntVal 5]
            
        it "duplicates top value with DUP" $ do
            stack <- runForth ["5", "DUP"]
            stack `shouldBe` [IntVal 5, IntVal 5]
            
        it "removes top value with DROP" $ do
            stack <- runForth ["5", "10", "DROP"]
            stack `shouldBe` [IntVal 5]
            
        it "swaps top two values with SWAP" $ do
            stack <- runForth ["5", "10", "SWAP"]
            stack `shouldBe` [IntVal 5, IntVal 10]
            
        it "copies second value with OVER" $ do
            stack <- runForth ["5", "10", "OVER"]
            stack `shouldBe` [IntVal 5, IntVal 10, IntVal 5]
            
        it "rotates top three values with ROT" $ do
            stack <- runForth ["5", "10", "15", "ROT"]
            stack `shouldBe` [IntVal 5, IntVal 15, IntVal 10]

    describe "Arithmetic operations" $ do
        it "adds two integers with +" $ do
            stack <- runForth ["5", "10", "+"]
            stack `shouldBe` [IntVal 15]
            
        it "subtracts with -" $ do
            stack <- runForth ["3", "10", "-"]
            stack `shouldBe` [IntVal 7]
            
        it "divides with /" $ do
            stack <- runForth ["2", "10", "/"]
            stack `shouldBe` [IntVal 5]
            
        it "raises to power with ^" $ do
            stack <- runForth ["2", "3", "^"]
            stack `shouldBe` [IntVal 9]
            
    describe "String operations" $ do
        it "converts to string with STR" $ do
            stack <- runForth ["42", "STR"]
            stack `shouldBe` [StringVal "42"]
            
        it "concatenates two strings with CONCAT2" $ do
            stack <- runForth ["\"World\"", "\"Hello \"", "CONCAT2"]
            stack `shouldBe` [StringVal "Hello World"]
            
        it "concatenates three strings with CONCAT3" $ do
            stack <- runForth ["\"!\"", "\"World\"", "\"Hello \"", "CONCAT3"]
            stack `shouldBe` [StringVal "Hello World!"]