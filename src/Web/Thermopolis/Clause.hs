{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving #-}

module Web.Thermopolis.Clause where

import		 Control.Applicative
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class

import           Data.Char
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import           Data.Text.Template

import           Network.CGI

import           Paths_thermopolis
import           Types
import qualified Data.Text as T

import           Web.Thermopolis.PageIdentity

class (Applicative f, Monad f) => ContentReader f where 
 readContent :: FilePath -> f Clause     -- ^ tell me how to load a static file

instance ContentReader IO where
  readContent fileName = do
        dir <- getDataDir
        LTIO.readFile $ dir ++ "/include/" ++ fileName
  
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
        substituteA (LT.toStrict f) context 
  where 
        context nm = case lookup nm env of
                      Nothing -> error $ "readClause failure for " ++ show nm ++ " in " ++ show filePath
                      Just g -> LT.toStrict <$> g

textToClause :: Text -> Clause
textToClause = LT.fromStrict 

-- We output with UTF-8.
outputClause :: MonadCGI m => Clause -> m CGIResult
outputClause = outputFPS . encodeUtf8 

classClause :: Class -> Clause
classClause = textToClause . T.pack . show

nbsp :: Applicative f => f Clause
nbsp = pure "&nbsp;"

infixl 4 <+>

(<+>) :: Applicative f => f Clause -> f Clause -> f Clause
(<+>) f g = mappend <$> f <*> g

rootClause :: (Functor f, PageIdentity p f) => f Clause
rootClause = textToClause <$> root
 
