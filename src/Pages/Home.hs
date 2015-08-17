{-# LANGUAGE OverloadedStrings #-}
module Pages.Home where

import           Config

import           Data.Monoid
import           Data.String

import           Pages.Utils

import           Types

data HomePage = HomePage
  { user :: User
  }

homePage :: ContentReader f => HomePage -> f Page
homePage env = do
 menu <- readPage "menu.html" 
         [("webRoot",fromString (webRoot config))]
 readPage "index.html" 
        [("who",Page ("Logged In as " <> fromString (show (user env))))
        ,("menu",menu)
        ,("content","{{NOTHING}}")
        ,("webRoot",fromString (webRoot config))
        ]
