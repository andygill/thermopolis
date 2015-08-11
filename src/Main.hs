{-# LANGUAGE OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving #-}

import           Control.Monad
import           Control.Natural
import           Control.Transformation
import           Crypto.PasswordStore

import           Data.List
import           Data.Monoid    (mconcat)
import qualified Data.Text as Text
import           Data.Text (Text)
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import           Data.Text.Template

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import           Control.Monad.IO.Class

import qualified Numeric 
import           Network.HTTP.Types.Status

import           Web.Scotty
import           Web.Scotty.Cookie
import           Web.Scotty.TLS

import           System.Entropy

tls :: ScottyM () -> IO ()
-- tls = scottyTLS 3000 "server.key" "server.crt"
tls = scotty 3000

main :: IO ()
main = tls $ do
         -- Anyone call view the login page
         get "/" $ do
             optKey <- getCookie "session-key"
             liftIO $ print optKey
             case optKey of
               Just key -> do
                 v <- liftIO $ res # HomePage key
                 -- TODO: what is we reject the key?
                 html v
               Nothing -> do
                 v <- liftIO $ res # WelcomePage
                 html v

         -- POSTing to / is the way a text password gets turned into a valid session key
         post "/" $ do
            email :: Text <- param "email"
            pass :: Text <- param "pass"
            liftIO $ print (email, pass)
            Just t <- liftIO $ res # NewSessionKey email pass
            setSimpleCookie "session-key" $ textOfSessionKey $ t
            redirect "/"

         getRawContent "css" ".css" "text/css; charset=utf-8"        res
         getRawContent "js"  ".js"  "text/javascript; charset=utf-8" res
         getRawContent "fonts"  ""  "application/octet-stream"       res

         notFound $ do
            t <- request
            liftIO $ print t
            return ()
   
   where
         res = Resources $ Nat $ \ x -> do 
            print x
            case x of
                 WelcomePage -> 
                         welcomePage
                 HomePage key -> do
                         LTIO.readFile "content/include/index.html"
                 RawContent fileName -> do
                         LBS.readFile $ "content/" ++ fileName
                 NewSessionKey userid pass -> do
                         sessionkey <- newSessionKey
                         return $ Just $ sessionkey



data Resources = Resources (ResourceM :~> IO)

instance Transformation ResourceM IO Resources where
  Resources t # f = t # f

type UserId = Text      -- a *ku* email address
type Pass   = Text

data ResourceM :: * -> * where
  WelcomePage :: ResourceM LT.Text
           -- ^ Generate the welcome page, including the password box
  HomePage :: SessionKeyText -> ResourceM LT.Text
           -- ^ Generate the homepage
  ClassPage :: SessionKeyText -> String -> ResourceM LT.Text
           -- ^ Generate a class-specific page
  ProjectPage :: SessionKeyText -> String -> String -> ResourceM LT.Text
           -- ^ Generate a project-specific page
  RawContent    :: FilePath             -> ResourceM LBS.ByteString
           -- ^ Return some raw content, typically from flat files
  NewSessionKey :: UserId -> Pass       -> ResourceM (Maybe SessionKey)
           -- ^ Generate a new session key with UserId's rights,
           --   if the Pass is valid,
           --   *or* fail for an undisclosed reason.

deriving instance Show (ResourceM a) 

        

{-
--class ServerLogger f where
-- notFound :: Request -> f Status

class ServerResources f where
 welcome :: f Text		-- ^ can always see the welcome


-- readJSONResource :: [Text] -> f Value
-- readHTMLResource :: [Text] -> f Value


evalResourceM :: ResourceM a -> IO a
evalResourceM (Welcome) = do
-}   


-----
type SessionKeyText = Text

newtype SessionKey = SessionKey BS.ByteString
        deriving (Eq, Ord, Show)

textOfSessionKey :: SessionKey -> Text
textOfSessionKey (SessionKey bs) = 
        Text.pack $ concatMap hex $ BS.unpack $ bs
 where hex = reverse . take 2 . reverse . ('0' :) . flip Numeric.showHex ""

newSessionKey :: IO SessionKey
newSessionKey = do
        es <- getEntropy 32
        return $ SessionKey es
        
checkSessionKey :: Text -> SessionKey -> Bool
checkSessionKey keyText key = keyText == textOfSessionKey key

        
getRawContent :: FilePath   -- first-level directory
              -> String     -- suffix to use
              -> LT.Text    -- style, for example "text/javascript; charset=utf-8"
              -> Resources
              -> ScottyM ()
getRawContent dir suff style res = do
    get (capture $ "/" ++ dir ++ "/:file") $ do
         fileName :: FilePath <- param "file"
         when (not (suff `isSuffixOf` fileName)) $ next
         t <- liftIO $ res # RawContent $ dir ++ "/" ++ fileName
         setHeader "Content-Type" $ style
         raw $ t

------------------------

class Monad f => ContentReader f where 
 readFileC :: FilePath -> f LT.Text

instance ContentReader IO where
 readFileC fileName = LTIO.readFile $ "content/" ++ fileName

--- 

welcomePage :: ContentReader f => f LT.Text
welcomePage = do
 login <- loginPage
 indexPage "Not Logged In" login "{{NOTHING}}"

loginPage :: ContentReader f => f LT.Text
loginPage = do
        f <- readFileC "include/login.html"
        return $ f

indexPage :: ContentReader f => LT.Text -> LT.Text -> LT.Text -> f LT.Text
indexPage who menu content = do
        f <- readFileC "include/index.html"
        substituteA (LT.toStrict f) context
  where
          context "who"     = return $ LT.toStrict $ who
          context "menu"    = return $ LT.toStrict $  menu
          context "content" = return $ LT.toStrict $  content
          context  _        = fail "indexPage"
