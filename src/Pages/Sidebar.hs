{-# LANGUAGE OverloadedStrings,GADTs #-}
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


data Sidebar p = Sidebar [SideItem p]

data SideItem p = SideItem p (Sidebar p)

mkSidebar :: User -> Sidebar SmartPath
mkSidebar u = Sidebar
 [ SideItem Home (Sidebar [])
 ]

sidebarClause :: (PageIdentity p f, p ~ SmartPath, ContentReader f) => Sidebar p -> f Clause
sidebarClause (Sidebar items) = readClause "sidebar.html" [ ("content", mconcat <$> sequenceA (map sideItem items)) ]
  where
          sideItem (SideItem p (Sidebar ps)) =
                  sideLink p (pure "...")

--  = Classes [(Class,Int)]        -- classname and number of homeworks

{-
sidebarClause :: (PageIdentity p f, p ~ SmartPath,      ContentReader f) => Sidebar -> f Clause
sidebarClause sidebar = readClause "sidebar.html" 
    [      ("content",mconcat <$> sequenceA
                  ([ sideLink (Home)
                            (glyphicon "home"
                             <+> nbsp 
                             <+> pure "Home")
                   ]  ++ concat
                   [ sideLink (AClass cls)
                              (glyphicon "education" <+> nbsp <+> return (textToClause $ T.pack $ show cls))  :
                     [ sideLink (AAssignment cls (HW j))
                                (nbsp  <+> nbsp  <+> nbsp  <+> nbsp <+> glyphicon "warning-sign"  
                                                 <+> nbsp <+> return (fromString ("Homework " ++ show j)))
                     | j <- [1..i]
                     ]
                   | (cls,i) <- clss
                   ])
           )
   ] where Classes clss = sidebar
-}

glyphicon :: ContentReader f => Text -> f Clause
glyphicon name = readClause "glyphicon.html" [("glyphicon",return $ "glyphicon glyphicon-" <> textToClause name)]


sideLink :: (PageIdentity p f, ContentReader f) => p -> f Clause -> f Clause
sideLink path content = readClause "sidelink.html" 
                   [ ("class",active path)
                   , ("path",rootClause <+> return (textToClause (T.pack (show path))))
--                   return $ viewRootClause path <> pathToClause (viewee path)) 
                   , ("content",content)
                   ]

active :: (Eq p, Functor f, PageIdentity p f) => p -> f Clause
active p = (\ s -> if s == p then "active" else "") <$> self


