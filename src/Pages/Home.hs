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
homePage home = do
 menu <- readPage "menu.html" []
 readPage "index.html"
        [("who",return $ Page ("Logged In as " <> fromString (show (user home))))
        ,("menu",readPage "menu.html" [])
        ,("content",return $ "{{NOTHING}}")
        ]
