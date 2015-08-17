{-# LANGUAGE OverloadedStrings #-}
module Pages.Welcome where

import           Config

import           Data.String

import           Pages.Utils


welcomePage :: ContentReader f => f Page
welcomePage = do
 login <- readPage "login.html" []
 readPage "index.html" 
        [("who","Not Logged In")
        ,("menu",login)
        ,("content","{{NOTHING}}")
        ,("webRoot",fromString (webRoot config))
        ]
