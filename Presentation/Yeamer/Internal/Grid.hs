-- |
-- Module      : Presentation.Yeamer.Internal.Grid
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Presentation.Yeamer.Internal.Grid where

import Data.Semigroup.Numbered

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)

import Control.Applicative (liftA2)

data Gridded a = GridRegion a
               | GridDivisions [[Gridded a]]
  deriving (Generic, Functor)
instance FromJSON a => FromJSON (Gridded a)
instance ToJSON a => ToJSON (Gridded a)

instance Applicative Gridded where
  pure = GridRegion
  fs <*> GridRegion x = ($x) <$> fs
  GridRegion f <*> xs = f <$> xs
  GridDivisions fs <*> GridDivisions xs = GridDivisions $ liftA2 (<*>) <$> fs <*> xs
instance Monad Gridded where
  return = GridRegion
  GridRegion x >>= f = f x
  GridDivisions xs >>= f = GridDivisions $ map (>>=f) <$> xs

instance SemigroupNo 0 (Gridded a) where
  sappendN _ (GridDivisions g) (GridDivisions h) | length g == length h
        = GridDivisions $ zipWith (++) g h
  sappendN p a b = sappendN p (GridDivisions [[a,b]]) b

instance SemigroupNo 1 (Gridded a) where
  sappendN _ (GridDivisions g@(l:_)) (GridDivisions h@(m:_)) | length l == length m
        = GridDivisions $ g++h
  sappendN p a b = sappendN p (GridDivisions [[a],[b]]) b
  
