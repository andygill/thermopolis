{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified PageInfo  
import qualified Config

import Network.CGI

import Pages.Utils
import Pages.Welcome
import View
import Debug(cgiDebug)

cgiMain :: CGI CGIResult
cgiMain = do
        p <- liftIO $ welcomePage (View { viewPath = []
                                        , viewee = () })
        outputClause p

main :: IO ()
main = runCGI $ handleErrors $ cgiMain


