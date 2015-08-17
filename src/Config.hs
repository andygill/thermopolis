{-# LANGUAGE OverloadedStrings #-}
module Config where
        
data Config = Config 
  { webRoot :: String
  }        
  
config :: Config
config = Config
  { webRoot = "/~andy/thermopolis/"
  }


