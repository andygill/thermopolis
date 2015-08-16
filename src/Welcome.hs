module Main where

import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as LT

import Network.CGI

import Pages.Utils
import Pages.Welcome
cgiMain :: CGI CGIResult
cgiMain = do
        Page v <- liftIO $ welcomePage
        outputFPS $ encodeUtf8 $ v

main :: IO ()
main = runCGI $ handleErrors $ cgiMain
