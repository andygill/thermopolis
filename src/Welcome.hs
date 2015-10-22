{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Reader

import Network.CGI

import Web.Thermopolis.Clause
import View.Welcome
import Debug()

cgiMain :: CGI CGIResult
cgiMain = do
        p <- liftIO $ runReaderT welcomePage ()
        outputClause p

main :: IO ()
main = runCGI $ handleErrors $ cgiMain


