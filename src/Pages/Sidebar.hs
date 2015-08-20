{-# LANGUAGE OverloadedStrings #-}
module Pages.Sidebar where

import           Data.Char        
import           Data.Monoid
import           Data.String
import           Pages.Utils
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

data Sidebar
  = Classes [(String,Int)]
  
sidebarPage :: ContentReader f => Sidebar -> f Page
sidebarPage (Classes clss) = readPage "sidebar.html" [
           ("content",mconcat <$> sequenceA
                  ([ sideLink (glyphicon "home"
                             <+> nbsp 
                             <+> return "Home")
                   ] ++ concat
                   [ sideLink  (glyphicon "education" <+> nbsp <+> return (fromString cls))  :
                     [ sideLink (nbsp  <+> nbsp  <+> nbsp  <+> nbsp <+> glyphicon "warning-sign"  
                                                     <+> nbsp <+> return (fromString ("Homework " ++ show j)))
                     | j <- [1..i]
                     ]
                   | (cls,i) <- clss
                   ])
           )
   ]


nbsp :: Applicative f => f Page
nbsp = pure "&nbsp;"

infixl 4 <+>

(<+>) :: Applicative f => f Page -> f Page -> f Page
(<+>) f g = mappend <$> f <*> g


glyphicon :: ContentReader f => String -> f Page
glyphicon name = readPage "glyphicon.html" [("glyphicon",return (fromString $ "glyphicon glyphicon-" ++ name))]

-- "triangle-bottom"    


-- TODO: add active
sideLink :: ContentReader f => f Page -> f Page
sideLink content = readPage "sidelink.html" 
                   [ ("class",return "")        -- active if *this* page
                   , ("url",return "#")
                   , ("content",content)
                   ]


{-                   
nestBar :: ContentReader f => String -> Page -> f Page -> f Page
nestBar tag header content = readPage "nestbar.html" 
           [ ("target",return $ textToId tag)
           , ("header",return header)
           , ("content",content)
           ]
-}

