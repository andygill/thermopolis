{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module Types where
        
import Control.Monad.Trans.Reader

import Data.Char 
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Control.Applicative ((<|>))

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


data StudentPath 
    = Home
    | AClass Class
    | AAssignment Class Assignment
  deriving (Eq, Ord, Read)

instance Show StudentPath where
  show Home                  = ""
  show (AClass cls)          = show cls
  show (AAssignment cls ass) = show cls ++ "/" ++ show ass

readStudentPath :: String -> Maybe StudentPath
readStudentPath p = case words (map slash p) of
   [] -> Just Home
   [cls] -> fmap AClass $ readClass cls
   [cls,ass] -> AAssignment <$> readClass cls
                            <*> readAssignment ass
   _ -> fail "readStudentPath failed"
  where slash '/' = ' '
        slash c   = c

studentPathDepth :: StudentPath -> Int
studentPathDepth (Home)            = 0
studentPathDepth (AClass _)        = 0
studentPathDepth (AAssignment _ _) = 1

instance Depth StudentPath where
  depth = studentPathDepth


data Path = StudentPath        StudentPath
          | AsStudentPath Text StudentPath      -- Access a student path, as professor
          | SummaryPath        StudentPath      -- The summaries for professor
   deriving Eq          

instance Show Path where
  show (StudentPath path)        = "home/" ++ show path
  show (AsStudentPath user path) = "home/~" ++ T.unpack user ++ "/" ++ show path
  show (SummaryPath path)        = "home/admin/" ++ show path


instance Depth Path where
  depth (StudentPath path)     = depth path + 1
  depth (AsStudentPath _ path) = depth path + 2
  depth (SummaryPath path)     = depth path + 2


readPath :: String -> Maybe Path
readPath xs = 
        StudentPath <$> match "home/" 
  where match txt = if txt `isPrefixOf` xs 
                    then readStudentPath (drop (length txt) xs)
                    else Nothing

----------------------------------------------------
{-
class PageIdentity p f | f -> p where
   self      :: p -> f Bool
   pageDepth :: f Int
   
instance PageIdentity () IO where
   self () = return True
   pageDepth = return 0
   
   
instance Monad f => PageIdentity StudentPath (ReaderT StudentPath f) where
   self p = (== p) <$> ask
   pageDepth = StudentPathDepth <$> ask
-}        