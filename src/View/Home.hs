{-# LANGUAGE OverloadedStrings, GADTs #-}
module View.Home where

import           Config
import           Control.Applicative

import           Data.Monoid
import           Data.String

import           Model.Page
import           Model.Home

import           Pages.Sidebar (Sidebar, sidebarClause)
import           Pages.Utils

import           Types
import           View
import           View.Page

import           Web.Thermopolis.PageIdentity


homePage :: (PageIdentity p f, p ~ SmartPath, ContentReader f) => Page HomeContent -> f Clause
homePage page = pageClause $ (\ HomeContent -> "... HA! ...") <$> page
