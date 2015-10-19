module Types where
        
import Data.Text (Text)

data User = User
        { userName :: Text
        , userClasses :: [Text]
        } deriving Show


-- 'Path' is a list of 'Text' of the absolute path for a specific webpage, from our root.
type Path = [Text]
