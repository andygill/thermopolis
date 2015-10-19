{-# LANGUAGE DeriveFunctor #-}

module View where

import Data.Text(Text)
import Types(Path)


-- A 'View' is a wrapper around an objected to be viewed, 
-- that contains presentation specific information, like
-- web root address, and path to this page.

data View a = View
  { viewPath :: Path    -- what is the (normalized) path to this page
  , viewRoot :: Text    -- URL to web root, installation specific
  , viewee   :: a
  } deriving Functor

