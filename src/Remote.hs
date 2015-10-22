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

openDB :: Text -> IO (Maybe RemoteDevice)
openDB _ = return $ return ()

data Remote :: * -> * where
        -- For a user, what classes are they in
        GetUserInfo     ::                                Remote [Class]
        -- For a class, what assignments are published
        GetHomeworks    :: Class ->                       Remote [Assignment]

        Bind            :: Remote a -> (a -> Remote b) -> Remote b
        Return          :: a ->                           Remote a

send ::  RemoteDevice -> Remote a -> IO a 
send d (Bind m k) = send d m >>= send d . k
send d (Return a) = return a
send d (GetUserInfo) = return $ [EECS581,EECS776]
send d (GetHomeworks EECS776) = return $ [HW i | i <- [1..3]]
send d (GetHomeworks EECS581) = return $ [HW i | i <- [1..4]]
send d (GetHomeworks EECS368) = return $ [HW i | i <- [1..1]]

instance Monad Remote where
    return = Return
    (>>=) = Bind
    
instance Applicative Remote where    
    pure = Return
    (<*>) = ap

instance Functor Remote where    
    fmap = ap . pure
