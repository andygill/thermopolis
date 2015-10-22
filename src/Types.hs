{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Types where
        
import Control.Monad.Trans.Reader

import Data.Char 
import Data.Text (Text)
import qualified Data.Text as T
import Data.List

import           Web.Thermopolis.PageIdentity

data User = User
        { userName :: Text
        , userClasses :: [Class]
        } deriving Show

data Class = EECS368 | EECS581 | EECS776
        deriving (Eq, Ord, Show, Read)

readClass :: String -> Maybe Class
readClass xs = case reads xs of
                [(c,"")] -> return c
                _ -> fail "readClass"

data Assignment = HW Int
        deriving (Eq, Ord)
        
instance Show Assignment where
    show (HW n) = "HW" ++ show n

instance Read Assignment where
    readsPrec _ xs0 = [ (v,xs1)
                      | (t,xs1) <- lex xs0
                      , let (Just v) = readAssignment t
                      ]

readAssignment :: String -> Maybe Assignment
readAssignment xs | length xs > 2 
                 && all isDigit (drop 2 xs)
                 && "hw" `isPrefixOf` (map toLower xs)
                 = return $ HW $ read $ drop 2 $ xs
readAssignment _ = fail "readAssignment"

data SmartPath = Home
               | AClass Class
               | AAssignment Class Assignment
        deriving (Eq, Ord, Read)

instance Show SmartPath where
  show Home                  = "home/"
  show (AClass cls)          = "home/" ++ show cls
  show (AAssignment cls ass) = "home/" ++ show cls ++ "/" ++ show ass

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
smartPathDepth (Home)            = 1
smartPathDepth (AClass _)        = 1
smartPathDepth (AAssignment _ _) = 2

instance Depth SmartPath where
  depth = smartPathDepth

----------------------------------------------------
{-
class PageIdentity p f | f -> p where
   self      :: p -> f Bool
   pageDepth :: f Int
   
instance PageIdentity () IO where
   self () = return True
   pageDepth = return 0
   
   
instance Monad f => PageIdentity SmartPath (ReaderT SmartPath f) where
   self p = (== p) <$> ask
   pageDepth = smartPathDepth <$> ask
-}        