{-# LANGUAGE OverloadedStrings, GADTs #-}
module View.Class where

import           Control.Applicative

import           Data.Monoid
import           Data.String

import           Model.Class
import           Model.Home
import           Model.Page

import           Web.Thermopolis.Clause

import           Types

import           View.Page

import           Web.Thermopolis.PageIdentity

classPageClause :: (PageIdentity p f, p ~ StudentPath, ContentReader f) => Page ClassContent -> f Clause
classPageClause page = pageClause $ (\ ClassContent -> "... HA! HA HA...") <$> page
