module Main where

import System.Environment
import Interpret
import Control.Monad (when)

-- Main function
main :: IO ()
main = do
    args <- getArgs
    if null args
        then repl  -- Start REPL if no file is provided
        else do
            contents <- readFile (head args)
            (stack, _) <- interpret contents
            when (not (null stack)) $ do
                putStrLn "Stack is not empty at end of execution:"
                putStrLn $ show $ reverse stack

-- Simple REPL
repl :: IO ()
repl = do
    putStrLn "FORTH REPL (type 'quit' to exit)"
    loop
  where
    loop = do
        putStr "> "
        input <- getLine
        when (input /= "quit") $ do
            (stack, _) <- interpret input
            when (not (null stack)) $ do
                putStrLn "Stack:"
                putStrLn $ show $ reverse stack
            loop