{-# LANGUAGE DeriveFunctor #-}
module Model.Page where
        
import Data.Text(Text)
import Data.Tree

import Types

-- Everything a Page needs, including sidebar options, etc.

data Page a = Page
  { pageUser    :: Text
  , pageSidebar :: Forest SmartPath
  , pageContent :: a
  } deriving Functor
  
mkSidebar :: User -> Forest SmartPath
mkSidebar usr = 
        [ pure Home 
        ] ++
        [ Node (AClass cls) 
            [ pure (AAssignment cls (HW i))
            | i <- [1..4]
            ]
        | cls <- userClasses usr
        ]
            
mkPage :: User -> a -> Page a
mkPage usr a = Page (userName usr) (mkSidebar usr) a
