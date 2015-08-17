{-# LANGUAGE OverloadedStrings #-}
module Pages.Welcome where

import           Config

import           Data.String

import           Pages.Utils


welcomePage :: ContentReader f => f Page
welcomePage = do
 let env = [("webRoot",fromString (webRoot config))]
 login <- readPage "login.html" []
 content <- readPage "home.html" env
 readPage "index.html" $ env ++
        [("who","Thermopolis")
        ,("menu",login)
        ,("content",content)
        ]
