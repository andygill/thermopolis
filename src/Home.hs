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

import Remote
import Types
import Debug(cgiDebug)
import View

main :: IO ()
main = runCGI $ handleErrors $ checkAuthentication

checkAuthentication :: CGI CGIResult
checkAuthentication = do
        mAuth <- authType        
        case mAuth of
           Nothing -> outputInternalServerError ["no auth found"]
           Just auth | map toLower auth == "basic" -> checkUsername
           _       -> outputInternalServerError ["auth provided not understood"]
                   
checkUsername :: CGI CGIResult
checkUsername = do
        mUser <- remoteUser        
        case mUser of
           Nothing -> outputInternalServerError ["no user found inside auth zone"]
           Just user -> do
                db <- liftIO $ openDB
                userInfo <- liftIO $ send db $ GetUserInfo (T.pack user)
                if T.null (userName userInfo)
                then outputInternalServerError ["user is not in any classes"]
                else generateAuthenticatedPage userInfo


generateAuthenticatedPage :: User -> CGI CGIResult
generateAuthenticatedPage user = do
        optPath <- getInput "path"
        case optPath of
          Nothing -> generateHomePage user
          Just path -> case words (map slash path) of
                         []         -> generateHomePage user
                         [class']    -> generateClassPage user
                         [class',hw] -> generateHomeworkPage user                         
                         _          -> outputInternalServerError ["misformed path"]
  where slash '/' = ' '
        slash c   = c

generateHomePage :: User -> CGI CGIResult
generateHomePage user = do
    p <- liftIO $ homePage (mkView ["home",""] (HomePage user (Classes [("EECS 776",3),("EECS 581",4)])))
--                           (PageInfo.PageInfo Config.config ("home/" <> T.pack path))
    outputClause p        
  where path = ""

generateClassPage :: User -> CGI CGIResult
generateClassPage = generateHomePage

generateHomeworkPage :: User -> CGI CGIResult
generateHomeworkPage = generateHomePage

{-
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
    p <- liftIO $ runPageM (homePage (HomePage (User (T.pack user) ["EECS776"]) (Classes [("EECS 776",3),("EECS 581",4)])))
                           (PageInfo.PageInfo Config.config ("home/" <> T.pack path))
    outputPage p
main2 _ _ _ = outputInternalServerError ["auth provided not understood"]
-}
