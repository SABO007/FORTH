module Eval where

import Val
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Char (chr)
import System.IO (hFlush, stdout)

-- Forward declaration to break the cycle
data EnvData

-- Type for our FORTH environment - use a newtype to break the recursion
newtype Env = Env (Map.Map String ([Val] -> StateT (Env, [Val]) IO ()))

-- Create an empty environment
emptyEnv :: Env
emptyEnv = Env Map.empty

-- Evaluate a single FORTH token
evalForth :: String -> StateT (Env, [Val]) IO ()
evalForth "." = do
    (env, stack) <- get
    case stack of
        [] -> liftIO $ putStrLn "Error: stack underflow"
        (x:xs) -> do
            liftIO $ print x
            put (env, xs)

evalForth "DUP" = do
    (env, stack) <- get
    case stack of
        [] -> liftIO $ putStrLn "Error: stack underflow"
        (x:xs) -> put (env, x:x:xs)

evalForth "DROP" = do
    (env, stack) <- get
    case stack of
        [] -> liftIO $ putStrLn "Error: stack underflow"
        (_:xs) -> put (env, xs)

evalForth "SWAP" = do
    (env, stack) <- get
    case stack of
        (x:y:xs) -> put (env, y:x:xs)
        _ -> liftIO $ putStrLn "Error: stack underflow"

evalForth "OVER" = do
    (env, stack) <- get
    case stack of
        (x:y:xs) -> put (env, y:x:y:xs)
        _ -> liftIO $ putStrLn "Error: stack underflow"

evalForth "ROT" = do
    (env, stack) <- get
    case stack of
        (x:y:z:xs) -> put (env, z:x:y:xs)
        _ -> liftIO $ putStrLn "Error: stack underflow"

-- Addition operator
evalForth "+" = do
    (env, stack) <- get
    case stack of
        (IntVal a:IntVal b:xs) -> put (env, IntVal (b + a):xs)
        (FloatVal a:FloatVal b:xs) -> put (env, FloatVal (b + a):xs)
        (IntVal a:FloatVal b:xs) -> put (env, FloatVal (b + fromIntegral a):xs)
        (FloatVal a:IntVal b:xs) -> put (env, FloatVal (fromIntegral b + a):xs)
        _ -> liftIO $ putStrLn "Error: invalid operands for +"

-- Subtraction operator
evalForth "-" = do
    (env, stack) <- get
    case stack of
        (IntVal a:IntVal b:xs) -> put (env, IntVal (b - a):xs)
        (FloatVal a:FloatVal b:xs) -> put (env, FloatVal (b - a):xs)
        (IntVal a:FloatVal b:xs) -> put (env, FloatVal (b - fromIntegral a):xs)
        (FloatVal a:IntVal b:xs) -> put (env, FloatVal (fromIntegral b - a):xs)
        _ -> liftIO $ putStrLn "Error: invalid operands for -"

-- Multiplication operator
evalForth "*" = do
    (env, stack) <- get
    case stack of
        (IntVal a:IntVal b:xs) -> put (env, IntVal (b * a):xs)
        (FloatVal a:FloatVal b:xs) -> put (env, FloatVal (b * a):xs)
        (IntVal a:FloatVal b:xs) -> put (env, FloatVal (b * fromIntegral a):xs)
        (FloatVal a:IntVal b:xs) -> put (env, FloatVal (fromIntegral b * a):xs)
        _ -> liftIO $ putStrLn "Error: invalid operands for *"

-- Division operator
evalForth "/" = do
    (env, stack) <- get
    case stack of
        (IntVal 0:_:_) -> liftIO $ putStrLn "Error: division by zero"
        (FloatVal 0:_:_) -> liftIO $ putStrLn "Error: division by zero"
        (IntVal a:IntVal b:xs) -> put (env, IntVal (b `div` a):xs)
        (FloatVal a:FloatVal b:xs) -> put (env, FloatVal (b / a):xs)
        (IntVal a:FloatVal b:xs) -> put (env, FloatVal (b / fromIntegral a):xs)
        (FloatVal a:IntVal b:xs) -> put (env, FloatVal (fromIntegral b / a):xs)
        _ -> liftIO $ putStrLn "Error: invalid operands for /"

-- Power operator
evalForth "^" = do
    (env, stack) <- get
    case stack of
        (IntVal a:IntVal b:xs) -> put (env, IntVal (b ^ a):xs)
        (FloatVal a:FloatVal b:xs) -> put (env, FloatVal (b ** a):xs)
        (IntVal a:FloatVal b:xs) -> put (env, FloatVal (b ** fromIntegral a):xs)
        (FloatVal a:IntVal b:xs) -> put (env, FloatVal (fromIntegral b ** a):xs)
        _ -> liftIO $ putStrLn "Error: invalid operands for ^"

-- EMIT: Print character corresponding to ASCII code
evalForth "EMIT" = do
    (env, stack) <- get
    case stack of
        (IntVal a:xs) -> do
            liftIO $ putChar (chr a)
            liftIO $ hFlush stdout  -- Ensure the character is printed immediately
            put (env, xs)
        _ -> liftIO $ putStrLn "Error: EMIT requires integer argument"

-- CR: Print newline
evalForth "CR" = do
    liftIO $ putStrLn ""
    return ()

-- STR: Convert top stack value to string
evalForth "STR" = do
    (env, stack) <- get
    case stack of
        (val:xs) -> put (env, StringVal (valToString val):xs)
        _ -> liftIO $ putStrLn "Error: stack underflow"

-- CONCAT2: Concatenate two strings
evalForth "CONCAT2" = do
    (env, stack) <- get
    case stack of
        (StringVal s1:StringVal s2:xs) -> put (env, StringVal (s2 ++ s1):xs)
        _ -> liftIO $ putStrLn "Error: CONCAT2 requires two string arguments"

-- CONCAT3: Concatenate three strings
evalForth "CONCAT3" = do
    (env, stack) <- get
    case stack of
        (StringVal s1:StringVal s2:StringVal s3:xs) -> put (env, StringVal (s3 ++ s2 ++ s1):xs)
        _ -> liftIO $ putStrLn "Error: CONCAT3 requires three string arguments"

-- Handle string literals
evalForth s
    | head s == '\"' && last s == '\"' = do
        (env, stack) <- get
        let str = init (tail s)  -- Remove quotes
        put (env, StringVal str : stack)
    | otherwise = case reads s of
        [(n, "")] -> do
            (env, stack) <- get
            put (env, IntVal n:stack)
        _ -> case reads s of
            [(f, "")] -> do
                (env, stack) <- get
                put (env, FloatVal f:stack)
            _ -> liftIO $ putStrLn $ "Unknown token: " ++ s