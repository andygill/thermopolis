module Model.Assignment where

import Model.Page
import Remote
import Types
        
-- The claspage is content free right now
data AssignmentContent = AssignmentContent

mkAssignmentPage :: User -> Class -> Assignment -> Remote k (Page AssignmentContent)
mkAssignmentPage user _ _ = mkPage user AssignmentContent
