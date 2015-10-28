{-# LANGUAGE DeriveFunctor #-}
module Model.Page where
        
import Data.Foldable
import Data.Traversable
import Data.Text(Text)
import Data.Tree

import Types
import Remote

-- Everything a Page needs, including sidebar options, etc.

data Page a = Page
  { pageUser    :: Text
  , pageSidebar :: Forest Path
  , pageContent :: a
  } deriving Functor
  
instance Foldable Page where
  foldr f z page = f (pageContent page) z

instance Traversable Page where
  traverse f (Page usr bar a) = Page usr bar <$> f a
  
mkSidebar :: User -> Forest Path
mkSidebar usr = fmap (fmap StudentPath) $
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

