module Interpret where

import Val
import Eval  -- Import Eval module
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import Data.List (words)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)

-- Interpret a FORTH program, returning the final stack and environment
interpret :: String -> IO ([Val], Env)
interpret input = do
    let tokens = words input
        initialEnv = emptyEnv
        initialStack = []
    (_, (finalEnv, finalStack)) <- runStateT (mapM_ Eval.evalForth tokens) (initialEnv, initialStack)
    return (finalStack, finalEnv)