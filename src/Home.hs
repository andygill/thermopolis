{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified PageInfo  
import qualified Config

import Data.Char
import Data.Monoid
import qualified Data.Text as T

import Network.CGI

import Pages.Utils
import Pages.Home
import Pages.Sidebar(Sidebar(..))
import Debug

import Types

cgiMain :: CGI CGIResult
cgiMain = do
        -- If this page is served, there better be a remoteUser
        mAuth <- authType
        mUser <- remoteUser
        path <- getInput "path"
        main2 mAuth mUser (normalize path)
  where
        normalize Nothing  = ""
        normalize (Just p) = reverse $ f $ reverse $ p
          where f ('/':cs) = f cs
                f cs       = cs

main2 :: Maybe String -> Maybe String -> String -> CGI CGIResult
main2 Nothing _ _ = outputInternalServerError ["no auth found"]
main2 _ Nothing _ = outputInternalServerError ["no user found inside auth zone"]
main2 (Just auth) (Just user) path | map toLower auth == "basic" = do
    p <- liftIO $ runPageM (homePage (HomePage (User user) (Classes [("EECS 776",3),("EECS 581",4)])))
                           (PageInfo.PageInfo Config.config ("home/" <> T.pack path))
    outputPage p
main2 _ _ _ = outputInternalServerError ["auth provided not understood"]

main :: IO ()
main = runCGI $ handleErrors $ 
--                cgiDebug
                cgiMain
