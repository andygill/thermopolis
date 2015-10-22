{-# LANGUAGE DeriveFunctor #-}
module Model.Page where
        
import Data.Text(Text)
import Data.Tree

import Types
import Remote

-- Everything a Page needs, including sidebar options, etc.

data Page a = Page
  { pageUser    :: Text
  , pageSidebar :: Forest StudentPath
  , pageContent :: a
  } deriving Functor
  
mkSidebar :: User -> Forest StudentPath
mkSidebar usr = 
        [ pure Home 
        ] ++
        [ Node (AClass cls) 
            [ pure (AAssignment cls (HW i))
            | i <- [1..4]
            ]
        | cls <- userClasses usr
        ]
            
mkPage :: User -> a -> Remote k (Page a)
mkPage usr a = return $ Page (userName usr) (mkSidebar usr) a
