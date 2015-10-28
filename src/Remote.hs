{-# LANGUAGE OverloadedStrings, GADTs, KindSignatures, DataKinds, StandaloneDeriving #-}
module Remote 
  ( Remote(GetUserInfo,GetHomeworks,GetHomework)
  , RemoteDevice
  , send
  , openStudentDB
  , Permission(..)
  ) where

import Data.Text(Text)
import Control.Monad

import Types
import Question

type RemoteDevice (k :: Permission) = ()

openStudentDB :: Text -> IO (Maybe (RemoteDevice 'Student))
openStudentDB _ = return $ return ()

data Permission
  = Open
  | Student
  | Professor

data Remote :: Permission -> * -> * where
        -- For a user, what classes are they in
        GetUserInfo     ::                                Remote k [Class]
        -- For a class, what assignments are published
        GetHomeworks    :: Class ->                       Remote k [Assignment]
        -- For a specific homework, get the template
        GetHomework     :: Class -> Assignment ->         Remote k [Question]


        GetAssignment   :: Assignment ->                  Remote 'Student ()

        -- scoped sudo for professor
        Become          :: Text -> Remote 'Student a ->   Remote 'Professor a
        -- Who is in a specific class
        InClass         :: Class ->                       Remote 'Professor [Text]


        Bind            :: Remote k a -> (a -> Remote k b) -> Remote k b
        Return          :: a ->                           Remote k a

send ::  RemoteDevice k -> Remote k a -> IO a 
send d (Bind m k) = send d m >>= send d . k
send d (Return a) = return a
send d (GetUserInfo) = return $ [EECS581,EECS776]
send d (GetHomeworks EECS776) = return $ [HW i | i <- [1..3]]
send d (GetHomeworks EECS581) = return $ [HW i | i <- [1..4]]
send d (GetHomeworks EECS368) = return $ [HW i | i <- [1..1]]
send d (GetHomework EECS776 (HW 1)) = return $ [ Prose "Hello, World!" ]
send d (GetHomework cls ass) = return $ error (show ("GetHomework" :: String,cls,ass))

instance Monad (Remote k) where
    return = Return
    (>>=) = Bind
    
instance Applicative (Remote k) where    
    pure = Return
    (<*>) = ap

instance Functor (Remote k) where    
    fmap = ap . pure
