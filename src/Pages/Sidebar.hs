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

data Sidebar
  = Classes [(Text,Int)]        -- classname and number of homeworks

sidebarClause :: ContentReader f => View Sidebar -> f Clause
sidebarClause sidebar = readClause "sidebar.html" 
    [      ("content",mconcat <$> sequenceA
                  ([ sideLink (["home"] <$ sidebar)
                            (glyphicon "home"
                             <+> nbsp 
                             <+> pure "Home")
                   ]  ++ concat
                   [ sideLink (["home",textToId cls] <$ sidebar)
                              (glyphicon "education" <+> nbsp <+> return (textToClause cls))  :
                     [ sideLink (["home",textToId cls,"HW" <> fromString (show j)] <$ sidebar)
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


sideLink :: ContentReader f => View Path -> f Clause -> f Clause
sideLink path content = readClause "sidelink.html" 
                   [ ("class",return $ active path)
                   , ("path",return $ viewRootClause path <> pathToClause (viewee path)) 
                   , ("content",content)
                   ]

active :: View Path -> Clause
active v | viewee v == viewPath v = "active"
         | otherwise              = ""

