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
             key <- getCookie "session-key"
             v <- liftIO $ res # HomePage key
             liftIO $ print key
             html v

         -- POSTing to / is the way a text password gets turned into a valid session key
         post "/" $ do
            email :: Text <- param "email"
            pass :: Text <- param "pass"
            liftIO $ print (email, pass)
            Just t <- liftIO $ res # NewSessionKey email pass
            setSimpleCookie "session-key" $ textOfSessionKey $ t
            redirect "/"


         getRawContent "css" ".css" "text/css; charset=utf-8" res

{-
          -- TODO: think about if this is the right thing to do (file -> Text -> Lazy.ByteString)
         -- NOTE: choice - do not require session key here.
         get "/css/:css" $ do
             fileName :: FilePath <- param "css"
             -- TODO: check if on whitelist
             t <- liftIO $ res # CSS_File fileName
             setHeader "Content-Type" 
             raw $ encodeUtf8 t
-}
 
         get "/js/:js" $ do
             fileName :: FilePath <- param "js"
             -- TODO: check if on whitelist
             t <- liftIO $ res # JS_File fileName
             setHeader "Content-Type" "text/javascript; charset=utf-8"
             raw $ encodeUtf8 t

         get "/fonts/:fonts" $ do
             fileName :: FilePath <- param "fonts"
             -- TODO: check if on whitelist
             t <- liftIO $ res # Font_File fileName
             setHeader "Content-Type" "application/octet-stream"
             raw $ t

         notFound $ do
            t <- request
            liftIO $ print t
            return ()
   
   where
         res = Resources $ Nat $ \ x -> do 
            print x
            case x of
                 HomePage Nothing -> do
                         LTIO.readFile "content/include/index.html"
                 HomePage (Just key) -> do
                         LTIO.readFile "content/include/index.html"
                 RawContent fileName -> do
                         LBS.readFile $ "content/" ++ fileName
                 CSS_File fileName -> do
                         LTIO.readFile $ "content/css/" ++ fileName
                 JS_File fileName -> do
                         LTIO.readFile $ "content/js/" ++ fileName
                 Font_File fileName -> do
                         LBS.readFile $ "content/fonts/" ++ fileName
                 NewSessionKey userid pass -> do
                         sessionkey <- newSessionKey
                         return $ Just $ sessionkey
                         
data Resources = Resources (ResourceM :~> IO)

instance Transformation ResourceM IO Resources where
  Resources t # f = t # f

{-

         get "/:word" $ do
             beam <- param "word"
             html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
-}

type UserId = Text      -- a *ku* email address
type Pass   = Text

data ResourceM :: * -> * where
  HomePage      :: Maybe SessionKeyText -> ResourceM LT.Text
  RawContent    :: FilePath             -> ResourceM LBS.ByteString
  CSS_File      :: FilePath             -> ResourceM LT.Text
  JS_File       :: FilePath             -> ResourceM LT.Text
  Font_File     :: FilePath             -> ResourceM LBS.ByteString
  NewSessionKey :: UserId -> Pass ->       ResourceM (Maybe SessionKey)
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

