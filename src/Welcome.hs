{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT

import Network.CGI

import Pages.Utils
import Pages.Welcome

cgiMain :: CGI CGIResult
cgiMain = do
        p <- liftIO $ welcomePage
--        fail "Bad Page"
        outputPage p

main :: IO ()
main = runCGI $ handleErrors $ cgiMain
