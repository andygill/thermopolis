module Types where
        
newtype User = User String

instance Show User where
  show (User nm) = nm
