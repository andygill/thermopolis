module Model.Assignment where

import Model.Page
import Question
import Remote
import Types

        
-- The claspage is content free right now
data AssignmentContent = AssignmentContent [Question]        

mkAssignmentPage :: User -> Class -> Assignment -> Remote k (Page AssignmentContent)
mkAssignmentPage user cls ass = do
        qs <- GetHomework cls ass
        mkPage user $ AssignmentContent qs
