{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, ScopedTypeVariables #-}
module View.Page where

import           Config
import           Control.Applicative

import           Data.Monoid
import           Data.String

import           Model.Page
import           Model.Home

--import           Pages.Sidebar (Sidebar, sidebarClause)
import           Pages.Utils

import           Types

import           Web.Thermopolis.PageIdentity
import           Data.Tree
import           Data.Text(Text)
import qualified Data.Text as T


pageClause :: (PageIdentity p f, p ~ SmartPath, ContentReader f) => Page Clause -> f Clause
pageClause page = do
 menu <- readClause "menu.html" []
 readClause "index.html"
        [("webRoot",rootClause)
        ,("who",return $ ("Logged In as " <> textToClause (pageUser page)))
        ,("menu",readClause "menu.html" [])
        ,("content",readClause "content.html" 
            [("sidebar", sidebarClause $ pageSidebar page)
            ,("content",return (pageContent page))
            ])
        ]



sidebarClause :: forall p f . (PageIdentity p f, p ~ SmartPath, ContentReader f) => Forest p -> f Clause
sidebarClause items = readClause "sidebar.html" [ ("content", mconcat <$> sequenceA (map (sideItem 0) items)) ]
  where
          sideItem :: SmartPath ~ p => Int -> Tree p -> f Clause
          sideItem d item = sideLink (rootLabel item) 
                             (spacing d <+> labelPath (rootLabel item))
                     <+> (mconcat <$> sequenceA (fmap (sideItem (d+1)) (subForest item)))
          spacing 0 = pure ""            
          spacing n = nbsp <+> nbsp <+> nbsp  <+> nbsp <+> spacing (n-1)

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

labelPath :: (Monad f, ContentReader f) => SmartPath -> f Clause
labelPath Home                     = glyphicon "home" <+> nbsp <+> pure "Home"
labelPath (AClass cls)             = glyphicon "education" <+> nbsp <+> pure (fromString (show cls))
labelPath (AAssignment cls (HW i)) = glyphicon "warning-sign" <+> nbsp <+> pure "Homework" <+> nbsp <+> pure (fromString (show i))

active :: (Eq p, Functor f, PageIdentity p f) => p -> f Clause
active p = (\ s -> if s == p then "active" else "") <$> self


