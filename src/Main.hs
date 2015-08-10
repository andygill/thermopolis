{-# LANGUAGE OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables #-}

import           Data.Monoid    (mconcat)
import           Data.Text
import           Data.Text.Lazy.Encoding
import           Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import           Control.Monad.IO.Class

import           Network.HTTP.Types.Status

import           Web.Scotty
import           Web.Scotty.TLS

import           Control.Natural
import           Control.Transformation

tls :: ScottyM () -> IO ()
-- tls = scottyTLS 3000 "server.key" "server.crt"
tls = scotty 3000

main :: IO ()
main = tls $ do
         -- Anyone call view the login page
         get "/" $ do
             v <- liftIO $ res # Welcome
             html v


         post "/" $ do
            email :: FilePath <- param "email"
            pass :: FilePath <- param "pass"
            liftIO $ print (email, pass)
            return ()
            

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
                         
data Resources = Resources (ResourceM :~> IO)

instance Transformation ResourceM IO Resources where
  Resources t # f = t # f

{-

         get "/:word" $ do
             beam <- param "word"
             html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
-}

data ResourceM :: * -> * where
  Welcome  ::             ResourceM LT.Text
  CSS_File :: FilePath -> ResourceM LT.Text

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