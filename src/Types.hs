module Types where
        
import Data.Text (Text)

data User = User
        { userName :: Text
        , userClasses :: [Text]
        } deriving Show

