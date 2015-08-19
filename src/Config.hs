{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.String
import Pages.Utils (BaseEnv(..))

-- The config is compile-time static information.

data Config = Config 
  { webRoot   :: String   -- web root, with trailing space
  , superUser :: String   -- Who can access the professor page
  , classes   :: [String] -- List of classes
  }        
  
config :: Config
config = Config
  { webRoot   = "/~andy/thermopolis/"
  , superUser = "andy"
  , classes   = ["EECS368","EECS581","EECS776"]
  }

instance BaseEnv Config where
  getBaseEnv c = [("webRoot",fromString (webRoot c))]
