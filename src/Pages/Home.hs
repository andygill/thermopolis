{-# LANGUAGE OverloadedStrings #-}
module Pages.Home where

import           Config
import           Control.Applicative

import           Data.Monoid
import           Data.String

import           Pages.Sidebar (Sidebar, sidebarClause)
import           Pages.Utils

import           Types
import           View

import           Web.Thermopolis.PageIdentity

-- Content
data HomePage = HomePage
  { user    :: User
  , sidebar :: Sidebar
  }

-- Form
homePage :: (PageIdentity p f, ContentReader f) => View HomePage -> f Clause
homePage home = do
 menu <- readClause "menu.html" []
 readClause "index.html"
        [("webRoot",rootClause)
        ,("who",return $ ("Logged In as " <> fromString (show (user (viewee home)))))
        ,("menu",readClause "menu.html" [])
        ,("content",readClause "content.html" 
            [("sidebar", sidebarClause (sidebar <$> home))
            ,("content",return "ha!") -- textToPage <$> meC)
            ])
        ]

-- sidebarPage