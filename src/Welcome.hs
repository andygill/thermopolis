{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config

import Network.CGI

import Pages.Utils
import Pages.Welcome

cgiMain :: CGI CGIResult
cgiMain = do
        p <- liftIO $ runPageM welcomePage config
        outputPage p

main :: IO ()
main = runCGI $ handleErrors $ cgiMain


