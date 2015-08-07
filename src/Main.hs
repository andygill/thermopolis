{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid    (mconcat)
import           Web.Scotty
import           Web.Scotty.TLS

tls :: ScottyM () -> IO ()
-- tls = scottyTLS 3000 "server.key" "server.crt"
tls = scotty 3000

main :: IO ()
main = tls $ do
         get "/:word" $ do
             beam <- param "word"
             html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
