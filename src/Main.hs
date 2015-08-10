{-# LANGUAGE OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables #-}

import           Control.Natural
import           Control.Transformation
import           Crypto.PasswordStore

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
             v <- liftIO $ res # Welcome
             key <- getCookie "session-key"
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

         -- TODO: think about if this is the right thing to do (file -> Text -> Lazy.ByteString)
         get "/css/:css" $ do
             fileName :: FilePath <- param "css"
             -- TODO: check if on whitelist
             t <- liftIO $ res # CSS_File fileName
             setHeader "Content-Type" "text/css; charset=utf-8"
             raw $ encodeUtf8 t

         notFound $ do
            t <- request
            liftIO $ print t
            return ()
   
   where
         res = Resources $ Nat $ \ x -> case x of
                 Welcome -> do
                         LTIO.readFile "content/index.html"
                 CSS_File fileName -> do
                         LTIO.readFile $ "content/css/" ++ fileName
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
  Welcome       ::             ResourceM LT.Text
  CSS_File      :: FilePath -> ResourceM LT.Text
  NewSessionKey :: UserId   -> Pass -> ResourceM (Maybe SessionKey)
           -- ^ Generate a new session key with UserId's rights,
           --   if the Pass is valid,
           --   *or* fail for an undisclosed reason.

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

newtype SessionKey = SessionKey BS.ByteString
        deriving (Eq, Ord, Show)

textOfSessionKey :: SessionKey -> Text.Text
textOfSessionKey (SessionKey bs) = 
        Text.pack $ concatMap hex $ BS.unpack $ bs
 where hex = reverse . take 2 . reverse . ('0' :) . flip Numeric.showHex ""

newSessionKey :: IO SessionKey
newSessionKey = do
        es <- getEntropy 32
        return $ SessionKey es
        
--checkSessionKey :: Text -> SessionKey -> Bool

        
