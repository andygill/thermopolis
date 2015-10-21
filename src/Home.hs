{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Reader

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
           Just auth | map toLower auth == "basic" -> checkDB
           _       -> outputInternalServerError ["auth provided not understood"]

-- Next, check that we can open the database                   
checkDB :: CGI CGIResult
checkDB = do
        db <- liftIO $ openDB
        checkUsername db

-- Next, check that we have a user, and look up their basic info.
checkUsername :: RemoteDevice -> CGI CGIResult
checkUsername db = do
        mUser <- remoteUser        
        case mUser of
           Nothing -> outputInternalServerError ["no user found inside auth zone"]
           Just user -> do
                userInfo <- liftIO $ send db $ GetUserInfo (T.pack user)
                if T.null (userName userInfo)
                then outputInternalServerError ["user is not in any classes"]
                else checkPath db userInfo

checkPath :: RemoteDevice -> User -> CGI CGIResult
checkPath db user = do
        optPath <- getInput "path"
        case optPath >>= readSmartPath of
                 Nothing -> outputInternalServerError ["no valid path found"]
                 Just path -> generateAuthenticatedPage db user path

generateAuthenticatedPage :: RemoteDevice -> User -> SmartPath -> CGI CGIResult
generateAuthenticatedPage db user path = do
   hws <- liftIO $ send db $ sequence $ map GetHomeworks $ userClasses $ user
   case path of
     _ -> generate homePage (HomePage user (Classes [(EECS776,3),(EECS581,4)]))
--  _ -> outputInternalServerError ["bad path: " ++ show path]
  where
    generate f v = do
            -- This is where we encode that the authenticated service
            -- all have the home prefix. The apache checks that everything,
            -- from home down, is authenticated.
            p <- liftIO $ runReaderT (f (mkView ("home":[]) v)) path
            outputClause p            
{-


         case path of
                 []          -> generateHomePage path user
                 [class']    -> generateClassPage path user
                 [class',hw] -> generateHomeworkPage path user                         
                 _          -> outputInternalServerError ["misformed path"]


generateHomePage :: User -> CGI CGIResult
generateHomePage user = do
    p <- liftIO $ homePage (mkView ["home"] (HomePage user (Classes [("EECS 776",3),("EECS 581",4)])))
--                           (PageInfo.PageInfo Config.config ("home/" <> T.pack path))
    outputClause p        
  where path = ""

generateClassPage :: User -> CGI CGIResult
generateClassPage = generateHomePage

generateHomeworkPage :: User -> CGI CGIResult
generateHomeworkPage = generateHomePage
-}

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
