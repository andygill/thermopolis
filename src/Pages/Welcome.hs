{-# LANGUAGE OverloadedStrings #-}
module Pages.Welcome where

import           Pages.Utils
import           Types
import           Web.Thermopolis.PageIdentity

welcomePage :: (PageIdentity _p f, ContentReader f) => f Clause
welcomePage = do
 readClause "index.html" $
        [("who",    return "Thermopolis")
        ,("menu",   readClause "login.html" [])
        ,("content",readClause "welcome.html" [])
        ,("webRoot",rootClause)
        ]
