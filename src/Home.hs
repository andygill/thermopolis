{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char

import Network.CGI

import Pages.Utils
import Pages.Home
import Debug

import Types

cgiMain :: CGI CGIResult
cgiMain = do
        -- If this page is served, there better be a remoteUser
        mAuth <- authType
        mUser <- remoteUser
        main2 mAuth mUser

main2 Nothing _ = outputInternalServerError ["no auth found"]
main2 _ Nothing = outputInternalServerError ["no user found inside auth zone"]
main2 (Just auth) (Just user) | map toLower auth == "basic" = do
    p <- liftIO $ homePage (HomePage (User user))
    outputPage p
main2 _ _ = outputInternalServerError ["auth provided not understood"]

main :: IO ()
main = runCGI $ handleErrors $ 
--                cgiDebug
                cgiMain
