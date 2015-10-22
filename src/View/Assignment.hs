{-# LANGUAGE OverloadedStrings, GADTs #-}
module View.Assignment where

import           Control.Applicative

import           Data.Monoid
import           Data.String

import           Model.Assignment
import           Model.Page

import           Web.Thermopolis.Clause

import           Types

import           View.Page

import           Web.Thermopolis.PageIdentity

assignmentPageClause :: (PageIdentity p f, p ~ StudentPath, ContentReader f) => Page AssignmentContent -> f Clause
assignmentPageClause page = pageClause $ (\ AssignmentContent -> "... homework..") <$> page
