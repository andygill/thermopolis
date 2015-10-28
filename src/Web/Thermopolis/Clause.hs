{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving #-}

module Web.Thermopolis.Clause where

import		 Control.Applicative
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class

import           Data.Char
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT
import           Data.Text.Template

import           Network.CGI

import           Paths_thermopolis
import           Types
import qualified Data.Text as T

import           Web.Thermopolis.PageIdentity

class (Applicative f, Monad f) => ContentReader f where 
 readContent :: FilePath -> f Text  -- ^ tell me how to load a static file

instance ContentReader IO where
  readContent fileName = do
        dir <- getDataDir
        TextIO.readFile $ dir ++ "/include/" ++ fileName
  
instance (ContentReader f) => ContentReader (ReaderT s f) where
  readContent = lift . readContent

-- A Clause is a fragment of HTML, and is user/viewer facing.
-- A Clause should not be compared against; instead compare
-- the data being used to generate the Clause.
type Clause = LT.Text

-- Create an identifier (remove the spaces)
textToId :: Text -> Text
textToId = Text.filter (not . isSpace)

readClause :: ContentReader f => FilePath -> [(Text,f Clause)] -> f Clause
readClause filePath env = do
        f <- readContent filePath
        substClause f env

substClause :: ContentReader f => Text -> [(Text,f Clause)] -> f Clause
substClause f env = substituteA f context 
  where 
        context nm = case lookup nm env of
                      Nothing -> error $ "readClause failure for " ++ show nm ++ " in " ++ show f
                      Just g -> LT.toStrict <$> g

textToClause :: Text -> Clause
textToClause = LT.concatMap toCER . LT.fromStrict 
  where toCER c | c == '&'  = "&amb;"
                | c == '"'  = "&quot;"
                | c == '<'  = "&lt;"
                | c == '>'  = "&gt;"
                | isAscii c = LT.pack [c]
                | otherwise = "&#" <> LT.pack (show (ord c)) <> ";"

showClause :: (Show a) => a -> Clause
showClause = textToClause . T.pack . show

-- We output with UTF-8.
outputClause :: MonadCGI m => Clause -> m CGIResult
outputClause = outputFPS . encodeUtf8 

nbsp :: Applicative f => f Clause
nbsp = pure "&nbsp;"

infixl 4 <+>

(<+>) :: Applicative f => f Clause -> f Clause -> f Clause
(<+>) f g = mappend <$> f <*> g

rootClause :: (Functor f, PageIdentity p f) => f Clause
rootClause = textToClause <$> root
 
