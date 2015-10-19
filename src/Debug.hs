module Debug where

import           Network.CGI

cgiDebug :: MonadCGI m => m CGIResult
cgiDebug = do
  setHeader "Content-type" "text/plain"
  ss <- sequence 
          [ return "Debugging Page"
          , getBody             >>= return . ("getBody: " ++) . show
          , getInputs           >>= return . ("getInputs: " ++) . show 
          , getVars             >>= return . ("getVars: " ++) . show 
          , serverName          >>= return . ("serverName: " ++) . show 
          , serverPort          >>= return . ("serverPort: " ++) . show 
          , requestMethod       >>= return . ("requestMethod: " ++) . show 
          , pathInfo       >>= return . ("pathInfo: " ++) . show 
          , pathTranslated       >>= return . ("pathTranslated: " ++) . show 
          , scriptName       >>= return . ("scriptName: " ++) . show 
          , queryString       >>= return . ("queryString: " ++) . show 
          , remoteHost            >>= return . ("remoteHost: " ++) . show 
          , remoteAddr            >>= return . ("remoteAddr: " ++) . show 
          , authType            >>= return . ("authType: " ++) . show 
          , remoteUser            >>= return . ("remoteUser: " ++) . show 
          , requestContentType            >>= return . ("requestContentType: " ++) . show 
          , requestContentLength            >>= return . ("requestContentLength: " ++) . show 
          , progURI            >>= return . ("progURI: " ++) . show 
          , queryURI            >>= return . ("queryURI: " ++) . show 
          , requestURI            >>= return . ("requestURI: " ++) . show 
          ]
  output $ unlines $ ss
