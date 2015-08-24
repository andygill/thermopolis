{-# LANGUAGE OverloadedStrings #-}
module Pages.Home where

import           Config
import           Control.Applicative

import           Data.Monoid
import           Data.String

import           Pages.Sidebar (Sidebar, sidebarPage)
import           Pages.Utils

import           Types


-- Content
data HomePage = HomePage
  { user    :: User
  , sidebar :: Sidebar
  }

-- Form
homePage :: ContentReader f => HomePage -> f Page
homePage home = do
 menu <- readPage "menu.html" []
 readPage "index.html"
        [("who",return $ Page ("Logged In as " <> fromString (show (user home))))
        ,("menu",readPage "menu.html" [])
        ,("content",readPage "content.html" 
            [("sidebar",sidebarPage (sidebar home))
            ,("content",textToPage <$> meC)
            ])
        ]

-- sidebarPage