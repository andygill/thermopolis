{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving #-}

module Pages.Utils where

import		 Control.Applicative
import           Control.Monad.Trans.Reader

import           Data.Char
import           Data.Monoid
import           Data.String
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import           Data.Text.Template

import           Network.CGI

import           Paths_thermopolis
import           Types
import           Data.List(intersperse)
import qualified Data.Text as T



class (Applicative f, Monad f) => ContentReader f where 
 readFileC :: FilePath -> f Clause     -- ^ tell me how to load a static file
 webRootC  :: f Text
 baseEnvC  :: f [(Text,Page)]           -- ^ tell me what the base context is
                                        --   (the webRoot, for example)

 meC :: f Text                          -- ^ Path of *this* page

class BaseEnv e where
   getWebRoot :: e -> Text
   getBaseEnv :: e -> [(Text,Text)]
   getMe      :: e -> Text

instance BaseEnv e => ContentReader (PageM e) where
 readFileC fileName = PageM $ do
--         print fileName
        dir <- liftIO $ getDataDir
        liftIO $ LTIO.readFile $ dir ++ "/include/" ++ fileName
 webRootC = PageM $ do
         e <- ask
         return (getWebRoot e)
 baseEnvC = PageM $ do
         e <- ask
         return [ (i,Page $ LT.fromStrict v) | (i,v) <- getBaseEnv e ]
 meC = PageM $ do
         e <- ask
         return (getMe e)

instance ContentReader IO where
  readFileC fileName = do
        dir <- getDataDir
        LTIO.readFile $ dir ++ "/include/" ++ fileName
  

-- return $ [("webRoot",fromString (webRoot config))]

newtype PageM e a = PageM (ReaderT e IO a)

runPageM :: PageM e a -> e -> IO a
runPageM (PageM m) = runReaderT m
 
deriving instance Functor     (PageM e)
deriving instance Applicative (PageM e)
deriving instance Monad       (PageM e)

-- TODO: Consider making this a synonym. We use Lazy Text,
-- because we use Page as Page Fragments.
newtype Page = Page LT.Text

-- A Clause is a fragment of HTML, and is user/viewer facing.
-- A Clause should not be compared against; instead compare
-- the data being used to generate the Clause.
type Clause = LT.Text

instance Show Page where
  show (Page i) = LT.unpack i
        
instance IsString Page where
  fromString = Page . fromString
  
instance Monoid Page where
  mempty = Page ""
  mappend (Page a) (Page b) = Page (a <> b)  
  
-- This uses a call-by-value semantics for the Pages, aka the 
-- string interpretation is done before injecting into a page.
readPage :: ContentReader f => FilePath -> [(Text,f Page)] -> f Page
readPage filePath env = do
        f <- readFileC filePath
        theWebRoot <- webRootC
        let baseEnv = [("webRoot",textToPage theWebRoot)]
        Page <$> substituteA (LT.toStrict f) (context baseEnv)
  where 
        context baseEnv nm = case lookup nm (env ++ ((\ (a,b) -> (a,return b)) <$> baseEnv)) of
            Just g -> (\ (Page f) -> LT.toStrict f) <$> g
            Nothing       -> fail $ "readPage " ++ show filePath ++ ", can not find var " ++ show nm

outputPage :: MonadCGI m => Page -> m CGIResult
outputPage (Page v) = outputFPS $ encodeUtf8 $ v

-- Create an identifier (remove the spaces)
textToId :: Text -> Text
textToId = Text.filter (not . isSpace)

textToPage :: Text -> Page
textToPage = Page . LT.fromStrict 

readClause :: ContentReader f => FilePath -> [(Text,f Clause)] -> f Clause
readClause filePath env = do
        f <- readFileC filePath
        let baseEnv = []
        substituteA (LT.toStrict f) context 
  where 
        context nm = case lookup nm env of
                      Nothing -> error $ "readClause failure for " ++ show nm ++ " in " ++ show filePath
                      Just g -> LT.toStrict <$> g

textToClause :: Text -> Clause
textToClause = LT.fromStrict 

-- A 'Path' is a list of (virtual) directories. A non-empty 'Path'
-- is always terminated with a "/".
pathToClause :: Path -> Clause
pathToClause = textToClause . T.concat . map (<> "/")

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

