{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving #-}

module Question where

import Web.Thermopolis.Clause

data Question 
        = Prose Clause          -- Some question
        | Header Int Clause     -- Heading
        | Textbox Int           -- Textbox of a specific size        
        | HorizontalRule        -- Dividing rule
        | Radio [Clause]        -- pick one-of
        | Check [Clause]        -- pick zero or more-of
                deriving Show

requestClause :: (ContentReader f) => Question -> f Clause
requestClause (Prose cls) = 
        readClause "<p>${content}</p>"
                [ ("content",pure cls)
                ]
requestClause (Header h cls) =
        substClause "<h1>${content}</h1>"
                [ ("content",pure cls)
                ]
requestClause other = 
        substClause "<pre>${content}</pre>"
                [ ("content",pure $ showClause other)
                ]
