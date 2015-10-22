module Model.Home where

import Model.Page
import Types
        
-- The homepage is content free right now
data HomeContent = HomeContent

mkHomePage :: User -> IO (Page HomeContent)
mkHomePage path = return $ mkPage path HomeContent
