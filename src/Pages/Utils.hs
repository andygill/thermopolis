{-# LANGUAGE OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving #-}

module Pages.Utils where

import           Config
import		 Control.Applicative

import           Data.String
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import           Data.Text.Template

import           Network.CGI

import           Paths_thermopolis

class (Applicative f, Monad f) => ContentReader f where 
 readFileC :: FilePath -> f LT.Text     -- ^ tell me how to load a static file
 baseEnvC  :: f [(Text,Page)]           -- ^ tell me what the base context is
                                        --   (the webRoot, for example)
 
instance ContentReader IO where
 readFileC fileName = do
--         print fileName
        dir <- getDataDir
        LTIO.readFile $ dir ++ "/include/" ++ fileName
 baseEnvC = return $ [("webRoot",fromString (webRoot config))]

newtype Page = Page LT.Text

instance IsString Page where
  fromString = Page . fromString

-- This uses a call-by-value semantics for the Pages, aka the 
-- string interpretation is done before injecting into a page.
readPage :: ContentReader f => FilePath -> [(Text,f Page)] -> f Page
readPage filePath env = do
        f <- readFileC filePath
        baseEnv <- baseEnvC
        Page <$> substituteA (LT.toStrict f) (context baseEnv)
  where 
        context baseEnv nm = case lookup nm (env ++ ((\ (a,b) -> (a,return b)) <$> baseEnv)) of
            Just g -> (\ (Page f) -> LT.toStrict f) <$> g
            Nothing       -> fail $ "readPage " ++ show filePath ++ ", can not find var " ++ show nm

outputPage :: MonadCGI m => Page -> m CGIResult
outputPage (Page v) = outputFPS $ encodeUtf8 $ v

