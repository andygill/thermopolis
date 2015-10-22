{-# LANGUAGE OverloadedStrings, GADTs #-}
module View.Home where

import           Control.Applicative

import           Data.Monoid
import           Data.String

import           Model.Page
import           Model.Home

import           Web.Thermopolis.Clause

import           Types

import           View.Page

import           Web.Thermopolis.PageIdentity


homePage :: (PageIdentity p f, p ~ SmartPath, ContentReader f) => Page HomeContent -> f Clause
homePage page = pageClause $ (\ HomeContent -> "... HA! ...") <$> page
