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
 let env = [("webRoot",fromString (webRoot config))]
 menu <- readPage "menu.html" env
 readPage "index.html" $ env ++
        [("who",Page ("Logged In as " <> fromString (show (user home))))
        ,("menu",menu)
        ,("content","{{NOTHING}}")
        ]
