{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Reader

import Data.Char
import Data.Monoid
import           Data.Text (Text)
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
           Just auth | map toLower auth == "basic" -> checkUsername
           _       -> outputInternalServerError ["auth provided not understood"]

-- Next, check that we have a user.
checkUsername :: CGI CGIResult
checkUsername = do
        mUser <- remoteUser        
        case mUser of
           Nothing -> outputInternalServerError ["no user found inside auth zone"]
           Just user -> initDB (T.pack user)
           
-- Next, check that we can open the database, and find out what clsses we are in.
initDB :: Text -> CGI CGIResult
initDB username = do
        optDB <- liftIO $ openDB username
        case optDB of
          Nothing -> outputInternalServerError ["error opening the database"]
          Just db -> do -- and look up their basic info.
                classes <- liftIO $ send db $ GetUserInfo
                if null classes
                then outputInternalServerError ["user is not in any classes"]
                else checkPath db (User username classes)

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
