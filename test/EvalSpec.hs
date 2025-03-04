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
    let initialEnv = emptyEnv
        initialStack = []
    (_, (_, finalStack)) <- runStateT (mapM_ evalForth tokens) (initialEnv, initialStack)
    return finalStack

-- Helper to run FORTH and get the final environment
runForthWithEnv :: [String] -> IO (Env, [Val])
runForthWithEnv tokens = do
    let initialEnv = emptyEnv
        initialStack = []
    (_, finalState) <- runStateT (mapM_ evalForth tokens) (initialEnv, initialStack)
    return finalState

main :: IO ()
main = hspec $ do
    describe "Basic FORTH operations" $ do
        it "pushes integers onto stack" $ do
            stack <- runForth ["5", "10"]
            stack `shouldBe` [IntVal 10, IntVal 5]
            
        it "pushes floating point numbers onto stack" $ do
            stack <- runForth ["5.5", "10.2"]
            stack `shouldBe` [FloatVal 10.2, FloatVal 5.5]
            
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
            
        it "adds integer and float with +" $ do
            stack <- runForth ["5", "10.5", "+"]
            stack `shouldBe` [FloatVal 15.5]
            
        it "subtracts with -" $ do
            stack <- runForth ["3", "10", "-"]
            stack `shouldBe` [IntVal 7]
            
        it "subtracts float from integer with -" $ do
            stack <- runForth ["2.5", "10", "-"]
            stack `shouldBe` [FloatVal 7.5]
            
        it "multiplies with *" $ do
            stack <- runForth ["4", "5", "*"]
            stack `shouldBe` [IntVal 20]
            
        it "multiplies integer and float with *" $ do
            stack <- runForth ["4", "5.5", "*"]
            stack `shouldBe` [FloatVal 22.0]
            
        it "divides with /" $ do
            stack <- runForth ["2", "10", "/"]
            stack `shouldBe` [IntVal 5]
            
        it "divides with float result using /" $ do
            stack <- runForth ["3", "10", "/"]
            stack `shouldBe` [IntVal 3]  -- Integer division
            
        it "divides float with /" $ do
            stack <- runForth ["2.0", "10.0", "/"]
            stack `shouldBe` [FloatVal 5.0]
            
        it "handles division by zero" $ do
            stack <- runForth ["0", "10", "/"]
            stack `shouldBe` [IntVal 10]  -- Original value remains, error is logged
            
        it "raises to power with ^" $ do
            stack <- runForth ["2", "3", "^"]
            stack `shouldBe` [IntVal 9]
            
        it "raises float to power with ^" $ do
            stack <- runForth ["2.0", "3.0", "^"]
            stack `shouldBe` [FloatVal 9.0]
            
    describe "String operations" $ do
        it "pushes string literals onto stack" $ do
            stack <- runForth ["\"Hello\"", "\"World\""]
            stack `shouldBe` [StringVal "World", StringVal "Hello"]
            
        it "converts to string with STR" $ do
            stack <- runForth ["42", "STR"]
            stack `shouldBe` [StringVal "42"]
            
        it "converts float to string with STR" $ do
            stack <- runForth ["42.5", "STR"]
            stack `shouldBe` [StringVal "42.5"]
            
        it "concatenates two strings with CONCAT2" $ do
            stack <- runForth ["\"World\"", "\"Hello \"", "CONCAT2"]
            stack `shouldBe` [StringVal "Hello World"]
            
        it "concatenates three strings with CONCAT3" $ do
            stack <- runForth ["\"!\"", "\"World\"", "\"Hello \"", "CONCAT3"]
            stack `shouldBe` [StringVal "Hello World!"]
            
        it "handles CONCAT2 with stack underflow" $ do
            stack <- runForth ["\"Hello\"", "CONCAT2"]
            stack `shouldBe` [StringVal "Hello"]  -- Original value remains, error is logged
            
    describe "User-defined functions" $ do
        it "defines and executes a simple function" $ do
            (env, stack) <- runForthWithEnv ["\"SQUARE\"", ":", "DUP", "*", ";"]
            let userFuncs = getUserFuncs env
            Map.member "SQUARE" userFuncs `shouldBe` True
            
        it "executes a user-defined function" $ do
            stack <- runForth ["\"SQUARE\"", ":", "DUP", "*", ";", "5", "SQUARE"]
            stack `shouldBe` [IntVal 25]
            
        it "composes multiple user-defined functions" $ do
            stack <- runForth [
                "\"SQUARE\"", ":", "DUP", "*", ";", 
                "\"CUBE\"", ":", "DUP", "SQUARE", "*", ";",
                "3", "CUBE"]
            stack `shouldBe` [IntVal 27]
            
    describe "Output operations" $ do
        it "removes top value after printing with ." $ do
            stack <- runForth ["5", "10", "."]
            stack `shouldBe` [IntVal 5]  -- 10 is printed and removed
            
        it "emits character with EMIT" $ do
            stack <- runForth ["65", "EMIT"]  -- ASCII 'A'
            stack `shouldBe` []  -- Value is consumed
            
    describe "Error handling" $ do
        it "handles stack underflow in DUP" $ do
            stack <- runForth ["DUP"]
            stack `shouldBe` []  -- Error is logged but program continues
            
        it "handles stack underflow in DROP" $ do
            stack <- runForth ["DROP"]
            stack `shouldBe` []  -- Error is logged but program continues
            
        it "handles stack underflow in SWAP" $ do
            stack <- runForth ["5", "SWAP"]
            stack `shouldBe` [IntVal 5]  -- Error is logged but original value remains
            
        it "handles stack underflow in arithmetic operations" $ do
            stack <- runForth ["+"]
            stack `shouldBe` []  -- Error is logged but program continues
            
        it "handles invalid type operations" $ do
            stack <- runForth ["\"Hello\"", "5", "+"]
            stack `shouldBe` [IntVal 5, StringVal "Hello"]  -- Error is logged but values remain