-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Main where

import Presentation.Yeamer.Internal.Grid
import Data.Semigroup.Numbered

import qualified Data.Aeson as JSON

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck (Arbitrary(..))

import Control.Monad


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
 [ testGroup "Grids"
   [ testGroup "Concatenation"
     [ testCase "Singletons, horizontal"
        $ pure 0 │ pure 1
           @?= GridDivisions [ [pure 0, pure 1] ]
     , testCase "Singletons, vertical"
        $ pure 1
            ──
          pure 2 @?= GridDivisions [ [pure 1]
                                   , [pure 2] ]
     , testCase "Quadrat"
        $ pure 0 │ pure 1
           ──
          pure 2 │ pure 3
          @?= GridDivisions [ [pure 0, pure 1]
                            , [pure 2, pure 3] ]
     , testCase "Rectangle"
        $ pure 0 │ pure 1 │ pure 4
           ──
          pure 2 │ pure 3 │ pure 5
          @?= GridDivisions [ [pure 0, pure 1, pure 4]
                            , [pure 2, pure 3, pure 5] ]
     ]
   , testGroup "JSON consistency"
     [ QC.testProperty "Parsing back structure"
        $ \g -> JSON.decode (JSON.encode g)
                == Just (g :: Gridded ())
     , QC.testProperty "Parsing back structure and values"
        $ \g -> JSON.decode (JSON.encode g)
                == Just (g :: Gridded Int)
     ]
   ]
 ]


instance (Arbitrary a) => Arbitrary (Gridded a) where
  arbitrary = do
    values <- arbitrary
    arbGrid values
   where arbGrid [] = do
           emptyDir <- arbitrary
           return . GridDivisions $ if emptyDir then [] else [[]]
         arbGrid [v] = return $ GridRegion v
         arbGrid values = GridDivisions <$> do
           [width,height] <- replicateM 2
                    $ (+1) . (`mod`maxSize) . QC.getNonNegative <$> arbitrary
           let portionSize = fromIntegral n / fromIntegral (width*height)
               portions = splitPortns portionSize 0 0 values
               rows = splitEach width portions
           forM rows $ mapM arbGrid
          where n = length values
                maxSize = floor $ fromIntegral n
                splitPortns _ _ _ [] = []
                splitPortns pnSize pos ideal l
                    = case splitAt thisSize l of
                        (portion, rest) ->
                          portion : splitPortns pnSize (pos+thisSize) (ideal+pnSize) rest
                 where thisSize
                        | fromIntegral pos > ideal  = floor pnSize
                        | otherwise                 = ceiling pnSize
                splitEach _ [] = []
                splitEach n l = case splitAt n l of
                   (portion,rest) -> portion : splitEach n rest
