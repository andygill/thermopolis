{-# LANGUAGE OverloadedStrings #-}
module View.Welcome where

import           Types

import           Web.Thermopolis.Clause
import           Web.Thermopolis.PageIdentity

welcomePage :: (PageIdentity _p f, ContentReader f) => f Clause
welcomePage = do
 readClause "index.html" $
        [("who",    return "Thermopolis")
        ,("title",pure "Thermopolis")
        ,("menu",   readClause "right-button.html" 
                        [ ("label",pure "Sign in")
                        , ("dest",pure "home/")
                        , ("webRoot",rootClause)
                        ])
        ,("content",readClause "welcome.html" [])
        ,("webRoot",rootClause)
        ]

data WelcomePath = WelcomePath
