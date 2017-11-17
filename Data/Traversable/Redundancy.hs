-- |
-- Module      : Data.Traversable.Redundancy
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Data.Traversable.Redundancy where


import Data.Foldable (toList)
import Data.Traversable

import Data.Vector (Vector)
import qualified Data.Vector as Arr

import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord (comparing)

import Control.Monad.Trans.State


rmRedundancy :: (Ord a, Traversable t) => t a -> (t Int, Vector a)
rmRedundancy q = ( (`evalState`indices) . forM q . const . state $ \((_,j):js) -> (j, js)
                 , resource )
 where backIxed = fmap ($ []) . Map.fromListWith (.) . zip (toList q) $ (:)<$>[0..]
       histogram = sortBy (comparing $ negate . length . snd)
                    $ Map.toList backIxed
       indices = sortBy (comparing fst)
                 [ (j,i)
                 | (i,js) <- zip [0..] $ snd<$>histogram
                 , j <- js ]
       resource = Arr.fromList $ fst<$>histogram
