{-# LANGUAGE OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving #-}

module Pages.Utils where

import           Control.Monad
import           Control.Natural
import           Control.Transformation
import           Crypto.PasswordStore

import           Data.List
import           Data.Monoid    (mconcat)
import           Data.String
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import           Data.Text.Template

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import           Control.Monad.IO.Class

import qualified Numeric 
import           Network.HTTP.Types.Status

import           Web.Scotty
import           Web.Scotty.Cookie
import           Web.Scotty.TLS

import           System.Entropy

class Monad f => ContentReader f where 
 readFileC :: FilePath -> f LT.Text

instance ContentReader IO where
 readFileC fileName = do
--         print fileName
         LTIO.readFile $ "include/" ++ fileName

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
            Nothing       -> fail $ "readPage " ++ show filePath ++ " for " ++ show nm
