{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

module View where

--import Data.Text(Text)
import           Data.List(intersperse)
import qualified Data.Text as T

import Pages.Utils(Clause, textToClause,pathToClause)

import Types(Path)

-- A 'View' is a wrapper around an objected to be viewed, 
-- that contains presentation specific information, like
-- web root address, and path to this page.

data View a = View
  { viewPath :: Path    -- what is the (normalized) path to this page
--  , viewRoot :: Text    -- URL to web root, installation specific
  , viewee   :: a
  } deriving (Functor, Show)

mkView :: Path -> a -> View a
mkView p a = View { viewPath = p, viewee = a }

-- | get the path to the root, assuming our current path
viewRootClause :: View a -> Clause
viewRootClause = textToClause . T.concat . map (const "../") . viewPath
 
viewPathClause ::  View a -> Clause
viewPathClause = pathToClause . viewPath
