{-# LANGUAGE OverloadedStrings, GADTs, KindSignatures #-}
module Remote 
  ( Remote(GetUserInfo,GetHomeworks)
  , RemoteDevice
  , send
  , openDB
  ) where

import Data.Text(Text)
import Types
import Control.Monad
        
type RemoteDevice = ()

openDB :: IO RemoteDevice
openDB = return ()

data Remote :: * -> * where
        GetUserInfo     :: Text ->                        Remote User
        GetHomeworks    :: Class ->                        Remote [Int]

        Bind            :: Remote a -> (a -> Remote b) -> Remote b
        Return          :: a ->                           Remote a

send ::  RemoteDevice -> Remote a -> IO a 
send d (Bind m k) = send d m >>= send d . k
send d (Return a) = return a
send d (GetUserInfo "andy") = return $ User "andy" [EECS581,EECS776]
send d (GetHomeworks EECS776) = return $ [1,2,3]
send d (GetHomeworks EECS581) = return $ [1,2,3,4]
send d (GetHomeworks EECS368) = return $ [1]

instance Monad Remote where
    return = Return
    (>>=) = Bind
    
instance Applicative Remote where    
    pure = Return
    (<*>) = ap

instance Functor Remote where    
    fmap = ap . pure
        