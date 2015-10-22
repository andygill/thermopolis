module Model.Home where

import Model.Page
import Remote
import Types
        
-- The homepage is content free right now
data HomeContent = HomeContent

mkHomePage :: User -> Remote k (Page HomeContent)
mkHomePage user = mkPage user HomeContent
