{-# LANGUAGE OverloadedStrings, TypeOperators, KindSignatures, GADTs, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving #-}

module Database.JSON where
        
import           Data.Aeson
import qualified Data.Text as Text
import           Data.Text (Text)

type Row      = Object
type NamedRow = Object -- ^ Object with an "id" field, of type Id, by construction.
type Id       = Text

newTable :: FilePath -> IO (CRUD a -> IO a)
newTable tableName = do


        return $ 
-}
  
-- Later, split this up
class CRUDr f where
  createRow :: Row       -> f NamedRow
  readRow   :: Id        -> f (Maybe NamedRow)
  updateRow :: NamedRow  -> f ()  -- what happens if this does not exist?
  deleteRow :: Id        -> f ()
  shutdown  :: Text      -> f ()
  -- ^ last message; please stop listening. Msg for informational purposes only.

data CRUD :: * -> * where
  CreateRow :: Row       -> CRUD NamedRow
  ReadRow   :: Id        -> CRUD (Maybe NamedRow)
  UpdateRow :: NamedRow  -> CRUD ()  -- what happens if this does not exist?
  DeleteRow :: Id        -> CRUD ()
  Shutdown  :: Text      -> CRUD ()

instance CRUDr CRUD where
  createRow = CreateRow
  


        