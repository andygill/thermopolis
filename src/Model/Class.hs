module Model.Class where

import Model.Page
import Remote
import Types
        
-- The claspage is content free right now
data ClassContent = ClassContent

mkClassPage :: User -> Class -> Remote k (Page ClassContent)
mkClassPage user _ = mkPage user ClassContent
