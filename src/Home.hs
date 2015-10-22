{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Reader

import Data.Char
import Data.Monoid
import qualified Data.Text as T

import           Model.Assignment
import           Model.Page
import           Model.Home
import           Model.Class

import Network.CGI

import Web.Thermopolis.Clause

import Remote
import Types
import Debug(cgiDebug)

import           View.Assignment
import           View.Class
import           View.Home


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
                classes <- liftIO $ send db $ GetUserInfo (T.pack user)
                if null classes
                then outputInternalServerError ["user is not in any classes"]
                else checkPath db (User (T.pack user) classes)

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
     Home          -> (liftIO $ send db $ mkHomePage user)      >>= generate . homePageClause
     AClass cls    -> (liftIO $ send db $ mkClassPage user cls) >>=  generate . classPageClause
     AAssignment cls ass 
                   -> (liftIO $ send db $ mkAssignmentPage user cls ass) 
                           >>=  generate . assignmentPageClause
     _             -> outputInternalServerError ["bad path: " ++ show path]
  where
    generate m = do
            -- This is where we encode that the authenticated service
            -- all have the home prefix. The apache checks that everything,
            -- from home down, is authenticated.
            p <- liftIO $ runReaderT m path
            outputClause p            
