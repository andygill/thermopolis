{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified PageInfo  
import qualified Config

import Network.CGI

import Pages.Utils
import Pages.Welcome

cgiMain :: CGI CGIResult
cgiMain = do
        p <- liftIO $ runPageM welcomePage (PageInfo.PageInfo Config.config "")
        outputPage p

main :: IO ()
main = runCGI $ handleErrors $ cgiMain


