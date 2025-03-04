module Eval where

import Val
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Char (chr)
import System.IO (hFlush, stdout)

-- Newtype for the environment to avoid cyclic dependency
newtype Env = Env (Map.Map String ([Val] -> StateT (Env, [Val]) IO ()),  -- Built-in operations
                     Map.Map String [String])  -- User-defined functions

-- Create an empty environment
emptyEnv :: Env
emptyEnv = Env (Map.empty, Map.empty)

-- Helper to get the built-in operations map
getBuiltins :: Env -> Map.Map String ([Val] -> StateT (Env, [Val]) IO ())
getBuiltins (Env (builtins, _)) = builtins

-- Helper to get the user-defined functions map
getUserFuncs :: Env -> Map.Map String [String]
getUserFuncs (Env (_, userFuncs)) = userFuncs

-- Helper to update the environment
updateEnv :: Env -> Map.Map String ([Val] -> StateT (Env, [Val]) IO ()) -> Map.Map String [String] -> Env
updateEnv _ newBuiltins newUserFuncs = Env (newBuiltins, newUserFuncs)

-- Evaluate a single FORTH token
evalForth :: String -> StateT (Env, [Val]) IO ()
evalForth ":" = defineFunction  -- Start function definition
evalForth ";" = return ()       -- End function definition (no-op, handled by defineFunction)
evalForth token
    | head token == '\"' && last token == '\"' = do
        -- Extract the string content (remove quotes)
        let str = init (tail token)
        (env, stack) <- get
        put (env, StringVal str : stack)

    | otherwise = do
    (env, stack) <- get
    let builtins = getBuiltins env
        userFuncs = getUserFuncs env
    case token of
        -- Built-in operations (unchanged)
        "." -> do
            case stack of
                [] -> liftIO $ putStrLn "Error: stack underflow"
                (x:xs) -> do
                    liftIO $ print x
                    put (env, xs)
        "DUP" -> do
            case stack of
                [] -> liftIO $ putStrLn "Error: stack underflow"
                (x:xs) -> put (env, x:x:xs)
        "DROP" -> do
            case stack of
                [] -> liftIO $ putStrLn "Error: stack underflow"
                (_:xs) -> put (env, xs)
        "SWAP" -> do
            case stack of
                (x:y:xs) -> put (env, y:x:xs)
                _ -> liftIO $ putStrLn "Error: stack underflow"
        "OVER" -> do
            case stack of
                (x:y:xs) -> put (env, y:x:y:xs)
                _ -> liftIO $ putStrLn "Error: stack underflow"
        "ROT" -> do
            case stack of
                (x:y:z:xs) -> put (env, z:x:y:xs)
                _ -> liftIO $ putStrLn "Error: stack underflow"
        "+" -> do
            case stack of
                (IntVal a:IntVal b:xs) -> put (env, IntVal (b + a):xs)
                (FloatVal a:FloatVal b:xs) -> put (env, FloatVal (b + a):xs)
                (IntVal a:FloatVal b:xs) -> put (env, FloatVal (b + fromIntegral a):xs)
                (FloatVal a:IntVal b:xs) -> put (env, FloatVal (fromIntegral b + a):xs)
                _ -> liftIO $ putStrLn "Error: invalid operands for +"
        "-" -> do
            case stack of
                (IntVal a:IntVal b:xs) -> put (env, IntVal (b - a):xs)
                (FloatVal a:FloatVal b:xs) -> put (env, FloatVal (b - a):xs)
                (IntVal a:FloatVal b:xs) -> put (env, FloatVal (b - fromIntegral a):xs)
                (FloatVal a:IntVal b:xs) -> put (env, FloatVal (fromIntegral b - a):xs)
                _ -> liftIO $ putStrLn "Error: invalid operands for -"
        "*" -> do
            case stack of
                (IntVal a:IntVal b:xs) -> put (env, IntVal (b * a):xs)
                (FloatVal a:FloatVal b:xs) -> put (env, FloatVal (b * a):xs)
                (IntVal a:FloatVal b:xs) -> put (env, FloatVal (b * fromIntegral a):xs)
                (FloatVal a:IntVal b:xs) -> put (env, FloatVal (fromIntegral b * a):xs)
                _ -> liftIO $ putStrLn "Error: invalid operands for *"
        "/" -> do
            case stack of
                (IntVal 0:_:_) -> liftIO $ putStrLn "Error: division by zero"
                (FloatVal 0:_:_) -> liftIO $ putStrLn "Error: division by zero"
                (IntVal a:IntVal b:xs) -> put (env, IntVal (b `div` a):xs)
                (FloatVal a:FloatVal b:xs) -> put (env, FloatVal (b / a):xs)
                (IntVal a:FloatVal b:xs) -> put (env, FloatVal (b / fromIntegral a):xs)
                (FloatVal a:IntVal b:xs) -> put (env, FloatVal (fromIntegral b / a):xs)
                _ -> liftIO $ putStrLn "Error: invalid operands for /"
        "^" -> do
            case stack of
                (IntVal a:IntVal b:xs) -> put (env, IntVal (b ^ a):xs)
                (FloatVal a:FloatVal b:xs) -> put (env, FloatVal (b ** a):xs)
                (IntVal a:FloatVal b:xs) -> put (env, FloatVal (b ** fromIntegral a):xs)
                (FloatVal a:IntVal b:xs) -> put (env, FloatVal (fromIntegral b ** a):xs)
                _ -> liftIO $ putStrLn "Error: invalid operands for ^"
        "EMIT" -> do
            case stack of
                (IntVal a:xs) -> do
                    liftIO $ putChar (chr a)
                    liftIO $ hFlush stdout
                    put (env, xs)
                _ -> liftIO $ putStrLn "Error: EMIT requires integer argument"
        "CR" -> do
            liftIO $ putStrLn ""
            return ()
        "STR" -> do
            case stack of
                (val:xs) -> put (env, StringVal (valToString val):xs)
                _ -> liftIO $ putStrLn "Error: stack underflow"
        "CONCAT2" -> do
            case stack of
                (StringVal s1:StringVal s2:xs) -> do
                    let result = StringVal (s2 ++ s1)
                    put (env, result : xs)
                [] -> liftIO $ putStrLn "Error: CONCAT2 requires two string arguments (stack empty)"
                [_] -> liftIO $ putStrLn "Error: CONCAT2 requires two string arguments (only one value on stack)"
                _ -> liftIO $ putStrLn "Error: CONCAT2 requires two string arguments (wrong types)"
        "CONCAT3" -> do
            case stack of
                (StringVal s1:StringVal s2:StringVal s3:xs) -> do
                    let result = StringVal (s3 ++ s2 ++ s1)
                    put (env, result : xs)
                _ -> liftIO $ putStrLn "Error: CONCAT3 requires three string arguments"
        -- Handle numbers and string literals (unchanged)
        _ -> case reads token of
            [(n, "")] -> put (env, IntVal n:stack)
            _ -> case reads token of
                [(f, "")] -> put (env, FloatVal f:stack)
                _ -> case Map.lookup token builtins of
                    Just op -> op stack  -- Call built-in operation
                    Nothing -> case Map.lookup token userFuncs of
                        Just func -> evalTokens func  -- Call user-defined function
                        Nothing -> liftIO $ putStrLn $ "Unknown token: " ++ token

-- Evaluate a list of tokens
evalTokens :: [String] -> StateT (Env, [Val]) IO ()
evalTokens = mapM_ evalForth

-- Define a new function
-- Define a new function
defineFunction :: StateT (Env, [Val]) IO ()
defineFunction = do
    liftIO $ putStr "Name: "
    liftIO $ hFlush stdout
    name <- liftIO getLine
    liftIO $ putStr "Body: "
    liftIO $ hFlush stdout
    bodyLine <- liftIO getLine
    let body = words bodyLine
        nameStr = name  -- Name as string
    
    (env, stack) <- get
    let userFuncs = getUserFuncs env
        newUserFuncs = Map.insert nameStr body userFuncs
        newEnv = updateEnv env (getBuiltins env) newUserFuncs
    put (newEnv, stack)  -- Keep the stack unchanged

-- Read the function body until ";"
readFunctionBody :: StateT (Env, [Val]) IO [String]
readFunctionBody = do
    (env, stack) <- get
    case stack of
        [] -> error "Unexpected end of input"
        (StringVal x:xs) -> do
            modify (\(e, _) -> (e, xs))
            if x == ";"
                then return []
                else do
                    rest <- readFunctionBody
                    return (x : rest)
        _ -> error "Expected string literal in function body"