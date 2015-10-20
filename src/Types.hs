{-# LANGUAGE OverloadedStrings #-}
module Types where
        
import Data.Char 
import Data.Text (Text)
import qualified Data.Text as T
import Data.List

data User = User
        { userName :: Text
        , userClasses :: [Class]
        } deriving Show


-- 'Path' is a list of 'Text' of the absolute path for a specific webpage, from our root.
type Path = [Text]

data Class = EECS368 | EECS581 | EECS776
        deriving (Eq, Ord, Show, Read)

readClass :: String -> Maybe Class
readClass xs = case reads xs of
                [(c,"")] -> return c
                _ -> fail "readClass"


data SmartPath = Home
               | AClass Class
               | AAssignment Class Assignment
        deriving (Eq, Ord, Show, Read)

data Assignment = HW Int
        deriving (Eq, Ord, Show, Read)

readAssignment :: String -> Maybe Assignment
readAssignment xs | length xs > 2 
                 && all isDigit (drop 2 xs)
                 && "hw" `isPrefixOf` (map toLower xs)
                 = return $ HW $ read $ drop 2 $ xs
readAssignment _ = fail "readAssignment"

readSmartPath :: String -> Maybe SmartPath
readSmartPath p = case words (map slash p) of
   [] -> Just Home
   [cls] -> fmap AClass $ readClass cls
   [cls,ass] -> AAssignment <$> readClass cls
                            <*> readAssignment ass
   _ -> fail "readSmartPath failed"
  where slash '/' = ' '
        slash c   = c

smartPathDepth :: SmartPath -> Int
smartPathDepth (Home)            = 0
smartPathDepth (AClass _)        = 1
smartPathDepth (AAssignment _ _) = 2
