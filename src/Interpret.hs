module Interpret where

import Val
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.List (words)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

-- Import these from your Eval module
import Eval (Env, emptyEnv, getBuiltins, getUserFuncs, updateEnv, evalForth)

-- Data type to track interpreter state
data InterpState = Normal | DefiningFunction String [String]

-- Interpret a FORTH program, returning the final stack and environment
interpret :: String -> IO ([Val], Env)
interpret input = do
    let tokens = words input
        initialEnv = emptyEnv
        initialStack = []
    (_, finalState) <- runStateT (evalTokensWithState tokens) (initialEnv, initialStack, Normal)
    let (finalEnv, finalStack, _) = finalState
    return (finalStack, finalEnv)

-- Evaluate tokens with state tracking
evalTokensWithState :: [String] -> StateT (Env, [Val], InterpState) IO ()
evalTokensWithState [] = return ()
evalTokensWithState (token:rest) = do
    (env, stack, state) <- get
    case state of
        Normal -> 
            if token == ":" then
                -- Start function definition
                case rest of
                    [] -> liftIO $ putStrLn "Error: expected function name after ':'"
                    (funcName:bodyTokens) -> do
                        put (env, stack, DefiningFunction funcName [])
                        evalTokensWithState bodyTokens
            else do
                -- Process a token normally
                result <- lift $ runStateT (evalForth token) (env, stack)
                let (_, (newEnv, newStack)) = result
                put (newEnv, newStack, Normal)
                evalTokensWithState rest
                
        DefiningFunction name body ->
            if token == ";" then do
                -- End function definition and save it
                let userFuncs = getUserFuncs env
                    newUserFuncs = Map.insert name (reverse body) userFuncs
                    newEnv = updateEnv env (getBuiltins env) newUserFuncs
                put (newEnv, stack, Normal)
                evalTokensWithState rest
            else do
                -- Collect function body tokens
                put (env, stack, DefiningFunction name (token:body))
                evalTokensWithState rest