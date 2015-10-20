{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module Web.Thermopolis.PageIdentity where

import Control.Monad.Trans.Reader

import           Data.Text(Text)
import qualified Data.Text as T

-- Every path has a depth, simply to find the way back to the root
class (Show p, Eq p) => Depth p where
   depth :: p -> Int

-- '()' is a placeholder, with depth of 0
instance Depth () where
  depth () = 0

class Depth p => PageIdentity p f | f -> p where
   self      :: f p

instance (Depth p, Monad f) => PageIdentity p (ReaderT p f) where
   self = ask

root :: (Functor f, PageIdentity p f) => f Text
root = T.concat . map (const "../") . enumFromTo 1 . depth <$> self
