{-# LANGUAGE OverloadedStrings #-}
module Pages.Sidebar where

import           Control.Applicative
import           Data.Char        
import           Data.Monoid
import           Data.String
import           Data.Traversable
import           Pages.Utils
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           View
import           Types

import           Web.Thermopolis.PageIdentity


data Sidebar
  = Classes [(Class,Int)]        -- classname and number of homeworks

sidebarClause :: (PageIdentity p f, ContentReader f) => View Sidebar -> f Clause
sidebarClause sidebar = readClause "sidebar.html" 
    [      ("content",mconcat <$> sequenceA
                  ([ sideLink (Home <$ sidebar)
                            (glyphicon "home"
                             <+> nbsp 
                             <+> pure "Home")
                   ]  ++ concat
                   [ sideLink (AClass cls <$ sidebar)
                              (glyphicon "education" <+> nbsp <+> return (textToClause $ T.pack $ show cls))  :
                     [ sideLink (AAssignment cls (HW j) <$ sidebar)
                                (nbsp  <+> nbsp  <+> nbsp  <+> nbsp <+> glyphicon "warning-sign"  
                                                 <+> nbsp <+> return (fromString ("Homework " ++ show j)))
                     | j <- [1..i]
                     ]
                   | (cls,i) <- clss
                   ])
           )
   ] where Classes clss = viewee sidebar


glyphicon :: ContentReader f => Text -> f Clause
glyphicon name = readClause "glyphicon.html" [("glyphicon",return $ "glyphicon glyphicon-" <> textToClause name)]


sideLink :: (PageIdentity p f, ContentReader f) => View SmartPath -> f Clause -> f Clause
sideLink path content = readClause "sidelink.html" 
                   [ ("class",return $ active path)
                   , ("path",rootClause <+> return (textToClause (T.pack (show (viewee path)))))
--                   return $ viewRootClause path <> pathToClause (viewee path)) 
                   , ("content",content)
                   ]

active :: View SmartPath -> Clause
active v = "" 
-- | viewee v == viewPath v = "active"
--         | otherwise              = ""

