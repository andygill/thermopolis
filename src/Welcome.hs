{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Reader

import qualified Config

import Network.CGI

import Web.Thermopolis.Clause
import Pages.Welcome
import Debug(cgiDebug)

cgiMain :: CGI CGIResult
cgiMain = do
        p <- liftIO $ runReaderT welcomePage ()
        outputClause p

main :: IO ()
main = runCGI $ handleErrors $ cgiMain


