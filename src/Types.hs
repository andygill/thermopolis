module Types where
        
import Data.Text (Text)

data User = User
        { userName :: Text
        , userClasses :: [Text]
        } deriving Show


-- 'Path' is a 'Text' of the absolute path for a specific webpage
type Path = Text
