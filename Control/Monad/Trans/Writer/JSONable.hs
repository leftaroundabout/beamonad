-- |
-- Module      : Control.Monad.Trans.Writer.JSONable
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Trans.Writer.JSONable where

import Data.Aeson
import GHC.Generics


data WriterT l m a = WriterT { runWriterT :: m (a, l) }
  deriving (Functor, Foldable, Traversable, Generic)
instance (ToJSON (m (a,l))) => ToJSON (WriterT l m a)
instance (FromJSON (m (a,l))) => FromJSON (WriterT l m a)
