{-# LANGUAGE OverloadedStrings, GADTs, KindSignatures #-}
module Remote 
  ( Remote(GetUserInfo)
  , RemoteDevice
  , send
  , openDB
  ) where

import Data.Text(Text)
import Types
        
type RemoteDevice = ()

openDB :: IO RemoteDevice
openDB = return ()

data Remote :: * -> * where
        GetUserInfo     :: Text ->                        Remote User
        Bind            :: Remote a -> (a -> Remote b) -> Remote b
        Return          :: a ->                           Remote a

send ::  RemoteDevice -> Remote a -> IO a 
send d (Bind m k) = send d m >>= send d . k
send d (Return a) = return a
send d (GetUserInfo "andy") = return $ User "andy" ["EECS776"]