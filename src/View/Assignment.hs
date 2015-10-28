{-# LANGUAGE OverloadedStrings, GADTs #-}
module View.Assignment where

import           Control.Applicative

import           Data.Monoid
import           Data.String

import           Model.Assignment
import           Model.Page

import           Question

import           Web.Thermopolis.Clause

import           Types

import           View.Page

import           Web.Thermopolis.PageIdentity

assignmentPageClause :: (PageIdentity p f, p ~ Path, ContentReader f) => Page AssignmentContent -> f Clause
assignmentPageClause page = traverse f page >>= pageClause
  where f :: ContentReader f => AssignmentContent -> f Clause
        f (AssignmentContent qs) = traverse requestClause qs >>= return . mconcat


--        pageClause $ (\ AssignmentContent -> "... homework..") <$> page
