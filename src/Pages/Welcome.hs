{-# LANGUAGE OverloadedStrings #-}
module Pages.Welcome where

import           Config

import           Data.String

import           Pages.Utils


welcomePage :: ContentReader f => f Page
welcomePage = do
 readPage "index.html" $ 
        [("who",return "Thermopolis")
        ,("menu",readPage "login.html" [])
        ,("content",readPage "welcome.html" [])
        ]
