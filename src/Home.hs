{-# LANGUAGE OverloadedStrings, DataKinds #-}
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
           Just user -> checkPath (T.pack user)
           
-- Next, check we have a valid path
checkPath :: Text -> CGI CGIResult
checkPath username = do
        optPath <- getInput "path"
        case optPath >>= readStudentPath of
                 Nothing -> outputInternalServerError ["no valid path found"]
                 Just path -> initDB username path


-- Next, check that we can open the database, and find out what clsses we are in.
initDB :: Text -> StudentPath -> CGI CGIResult
initDB username path = do
        optDB <- liftIO $ openStudentDB username
        case optDB of
          Nothing -> outputInternalServerError ["error opening the database"]
          Just db -> do -- and look up their basic info.
                classes <- liftIO $ send db $ GetUserInfo
                if null classes
                then outputInternalServerError ["user is not in any classes"]
                else generateAuthenticatedPage db (User username classes) path

generateAuthenticatedPage :: RemoteDevice 'Student -> User -> StudentPath -> CGI CGIResult
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
