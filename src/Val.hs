module Val where

-- FORTH values
data Val = IntVal Int 
         | FloatVal Float 
         | StringVal String 
         | BoolVal Bool
         | NilVal
         deriving (Eq)

-- Show instance for Val to properly display values
instance Show Val where
    show (IntVal i) = show i
    show (FloatVal f) = show f
    show (StringVal s) = s
    show (BoolVal True) = "TRUE"
    show (BoolVal False) = "FALSE"
    show NilVal = "NIL"

-- Convert a Val to String representation
valToString :: Val -> String
valToString = show  -- Reuse the Show instance

-- Convert an Int to Val
intToVal :: Int -> Val
intToVal = IntVal

-- Convert a Float to Val
floatToVal :: Float -> Val
floatToVal = FloatVal

-- Convert a String to Val
stringToVal :: String -> Val
stringToVal = StringVal

-- Convert a Bool to Val
boolToVal :: Bool -> Val
boolToVal = BoolVal

-- Safe extraction functions
getInt :: Val -> Maybe Int
getInt (IntVal i) = Just i
getInt _ = Nothing

getFloat :: Val -> Maybe Float
getFloat (FloatVal f) = Just f
getFloat _ = Nothing

getString :: Val -> Maybe String
getString (StringVal s) = Just s
getString _ = Nothing

getBool :: Val -> Maybe Bool
getBool (BoolVal b) = Just b
getBool _ = Nothing

-- Check if a Val is Nil
isNil :: Val -> Bool
isNil NilVal = True
isNil _ = False