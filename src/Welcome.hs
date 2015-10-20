{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Reader

import qualified PageInfo  
import qualified Config

import Network.CGI

import Pages.Utils
import Pages.Welcome
import View
import Debug(cgiDebug)

cgiMain :: CGI CGIResult
cgiMain = do
        p <- liftIO $ runReaderT welcomePage ()
        outputClause p

main :: IO ()
main = runCGI $ handleErrors $ cgiMain


