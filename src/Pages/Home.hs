{-# LANGUAGE OverloadedStrings #-}
module Pages.Home where

import           Config
import           Control.Applicative

import           Data.Monoid
import           Data.String

import           Pages.Sidebar (Sidebar, sidebarPage)
import           Pages.Utils

import           Types
import           View

-- Content
data HomePage = HomePage
  { user    :: User
  , sidebar :: Sidebar
  }

-- Form
homePage :: ContentReader f => View HomePage -> f Clause
homePage home = do
 menu <- readClause "menu.html" []
 readClause "index.html"
        [("webRoot",return $ viewRootClause home)
        ,("who",return $ ("Logged In as " <> fromString (show (user (viewee home)))))
        ,("menu",readClause "menu.html" [])
        ,("content",readClause "content.html" 
            [("sidebar",return "") -- sidebarPage (sidebar home))
            ,("content",return "ha!") -- textToPage <$> meC)
            ])
        ]

-- sidebarPage