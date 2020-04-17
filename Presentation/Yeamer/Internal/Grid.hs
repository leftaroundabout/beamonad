-- |
-- Module      : Presentation.Yeamer.Internal.Grid
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Presentation.Yeamer.Internal.Grid where

import Data.Semigroup.Numbered

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import Data.Flat (Flat)

import Data.Ratio ((%))

import Data.List (sortBy)
import Data.Ord (comparing)

import Control.Applicative (liftA2)
import Control.Arrow (second)

import Lens.Micro
import Lens.Micro.TH

data Gridded a = GridRegion a
               | GridDivisions [[Gridded a]]
  deriving (Generic, Functor, Eq, Show, Foldable, Traversable)
instance FromJSON a => FromJSON (Gridded a)
instance ToJSON a => ToJSON (Gridded a)
instance Flat a => Flat (Gridded a)

instance Applicative Gridded where
  pure = GridRegion
  fs <*> GridRegion x = ($ x) <$> fs
  GridRegion f <*> xs = f <$> xs
  GridDivisions fs <*> GridDivisions xs = GridDivisions $ liftA2 (<*>) <$> fs <*> xs
instance Monad Gridded where
  return = GridRegion
  GridRegion x >>= f = f x
  GridDivisions xs >>= f = GridDivisions $ map (>>=f) <$> xs

instance SemigroupNo 0 (Gridded a) where
  sappendN _ (GridDivisions g) (GridDivisions h) | length g == length h
        = GridDivisions $ zipWith (++) g h
  sappendN _ e (GridDivisions [r]) = GridDivisions [e:r]
  sappendN _ (GridDivisions [r]) e = GridDivisions [r++[e]]
  sappendN p a b = GridDivisions [[a,b]]

instance SemigroupNo 1 (Gridded a) where
  sappendN _ (GridDivisions g@(l:_)) (GridDivisions h@(m:_)) | length l == length m
        = GridDivisions $ g++h
  sappendN _ e (GridDivisions c@([r]:_)) = GridDivisions $ [e]:c
  sappendN _ (GridDivisions c@([r]:_)) e = GridDivisions $ c++[[e]]
  sappendN p a b = GridDivisions [[a],[b]]
  

data GridRange = GridRange {
      _xBegin, _xEnd, _yBegin, _yEnd :: Int }
   deriving (Eq, Show, Generic)
makeLenses ''GridRange
data GridLayout a = GridLayout {
      _gridWidth, _gridHeight :: Int
    , _gridContents :: [(GridRange, a)]
    } deriving (Functor, Generic, Eq, Show)
makeLenses ''GridLayout

layoutGrid :: Gridded a -> GridLayout a
-- layoutGrid = fmap snd . fst . layoutGridP

type GridRegionId = Int

layoutGridP :: Gridded a -> ( GridLayout (GridRegionId, a)
                            , [(GridRegionId, b)] -> Gridded b )
layoutGridP g = ( layoutGrid g & gridContents %~ zipWith (second . (,)) [0..]
                , const undefined )

layoutGrid (GridRegion a) = GridLayout 1 1 [(GridRange 0 1 0 1, a)]
layoutGrid (GridDivisions []) = GridLayout 0 0 []
layoutGrid (GridDivisions [row])
    = alignLayoutDirectional gridWidth xBegin xEnd
                             gridHeight yBegin yEnd
                        $ layoutGrid <$> row
layoutGrid (GridDivisions rows)
    = alignLayoutDirectional gridHeight yBegin yEnd
                             gridWidth xBegin xEnd
                        $ layoutGrid . GridDivisions . pure <$> rows

alignLayoutDirectional
    :: Lens' (GridLayout a) Int -> Lens' GridRange Int -> Lens' GridRange Int
    -> Lens' (GridLayout a) Int -> Lens' GridRange Int -> Lens' GridRange Int
    -> [GridLayout a] -> GridLayout a
alignLayoutDirectional gridLength sBegin sEnd
                       gridThickness zBegin zEnd
                           = align . map (\(ζ, h') -> ((0,h'), (h',(ζ,0))))
                                                  . xcat 0
 where align state = case sortBy (comparing $ snd . fst) state of
           (headSnail@((_,ySnail), _) : others)
             | ySnail < 1
               -> case break ((>ySnail) . snd . fst) others of
                    (snails, hares)
                      -> align $
                            [ ((ySnail, ySnail+h'), (h', (ζ,i+1)))
                            | (_, (h', (ζ,i))) <- headSnail : snails ]
                         ++ [ ((ySnail,yHare), (h', shiftup cH))
                            | ((_,yHare), (h', cH)) <- hares ]
           _   -> gather $ fst . snd . snd <$> state
       shiftup (ζ, i)
          = ( ζ & gridThickness %~ (+1)
                & gridContents . mapped
                         %~ \(range, a) -> (range & zBegin%~shift
                                                  & zEnd%~shift  , a)
            , i+1 )
        where shift j | j>i        = j+1
                      | otherwise  = j
       xcat _ [] = []
       xcat ix (ζ : cells)
          = ( ζ & gridContents . mapped . _1 %~ (sBegin %~(+ix))
                                              . (sEnd %~(+ix))
            , 1%(ζ^.gridThickness) )
              : xcat (ix + ζ^.gridLength) cells
       gather [ζ] = ζ
       gather (ζ₀ : others) = case gather others of
               ζo | ζ₀^.gridThickness == ζo^.gridThickness
                    -> ζo & gridLength %~ (ζ₀^.gridLength +)
                          & gridContents %~ (ζ₀^.gridContents ++)
