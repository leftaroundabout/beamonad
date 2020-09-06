-- |
-- Module      : Control.Monad.Trans.Writer.JSONable
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
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
import Flat
import GHC.Generics
import Data.Semigroup


data WriterT l m a = WriterT { runWriterT :: m (a, l) }
  deriving (Functor, Foldable, Traversable, Generic)
instance (ToJSON (m (a,l))) => ToJSON (WriterT l m a)
instance (FromJSON (m (a,l))) => FromJSON (WriterT l m a)
instance (Flat (m (a,l))) => Flat (WriterT l m a)
instance (Semigroup (m (a,l))) => Semigroup (WriterT l m a) where
  WriterT x <> WriterT y = WriterT $ x<>y
instance (Semigroup (m (a,l)), Monoid (m (a,l))) => Monoid (WriterT l m a) where
  mempty = WriterT mempty
  mappend = (<>)
