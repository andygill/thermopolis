{-# LANGUAGE OverloadedStrings #-}
module Pages.Welcome where

import           Pages.Utils
import           View

welcomePage :: ContentReader f => View () -> f Clause
welcomePage v = do
 readClause "index.html" $
        [("who",    return "Thermopolis")
        ,("menu",   readClause "login.html" [])
        ,("content",readClause "welcome.html" [])
        ,("webRoot",return $ textToClause $ viewRoot v)
        ]
