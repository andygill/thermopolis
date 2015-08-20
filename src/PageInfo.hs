{-# LANGUAGE OverloadedStrings #-}
module PageInfo where
        
import qualified Config as C
import           Data.String
import           Data.Text
import           Pages.Utils (BaseEnv(..))

data PageInfo = PageInfo
   { config :: C.Config
   , me     :: Text     -- my path, from root, with no leading, or trailing, slash. 
   }


instance BaseEnv PageInfo where
  getBaseEnv c = [("webRoot", C.webRoot $ config c)]
  getMe = me