{-# LANGUAGE OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving #-}

module Pages.Utils where

import           Data.String
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import           Data.Text.Template

import           Network.CGI

import           Paths_thermopolis

class Monad f => ContentReader f where 
 readFileC :: FilePath -> f LT.Text

instance ContentReader IO where
 readFileC fileName = do
--         print fileName
        dir <- getDataDir
        LTIO.readFile $ dir ++ "/include/" ++ fileName

newtype Page = Page LT.Text

instance IsString Page where
  fromString = Page . fromString

-- This uses a call-by-value semantics for the Pages, aka the 
-- string interpretation is done before injecting into a page.
readPage :: ContentReader f => FilePath -> [(Text,Page)] -> f Page
readPage filePath env = do
        f <- readFileC filePath
        Page <$> substituteA (LT.toStrict f) context
  where 
        context nm = case lookup nm env of
            Just (Page f) -> return $ LT.toStrict $ f
            Nothing       -> fail $ "readPage " ++ show filePath ++ ", can not find var " ++ show nm

outputPage :: MonadCGI m => Page -> m CGIResult
outputPage (Page v) = outputFPS $ encodeUtf8 $ v

