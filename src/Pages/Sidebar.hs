{-# LANGUAGE OverloadedStrings #-}
module Pages.Sidebar where

import           Data.Char        
import           Data.Monoid
import           Pages.Utils
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

data Sidebar
  = Classes [(String,Int)]
  
sidebarPage :: ContentReader f => Sidebar -> f Page
sidebarPage (Classes _) = readPage "sidebar.html" [
           ("content",mconcat <$> sequenceA
                  [ sideLink "home" "Home!" 
                  , sideLink "home" "Home!"
                  ]
           )
   ]

-- TODO: add active
sideLink :: ContentReader f => Page -> Page -> f Page
sideLink glyphicon content = readPage "sidelink.html" 
                   [ ("class",return "")
                   , ("glyphicon",return glyphicon)
                   , ("content",return content)
                   ]
                   
--nestBar :: ContentReader f => Text -> Page -> f Page -> f Page
--nestBar tag header = 

