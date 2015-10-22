{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Text
import Web.Thermopolis.Clause -- (BaseEnv(..))

-- The config is compile-time static information.

data Config = Config 
  { webRoot   :: Text           -- web root, *with* trailing /
  , superUser :: String         -- Who can access the professor page
  , classes   :: [String]       -- List of classes
  }        
  
config :: Config
config = Config
  { webRoot   = "/~andy/thermopolis/"
  , superUser = "andy"
  , classes   = ["EECS368","EECS581","EECS776"]
  }
