-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
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
           =#?@= ( GridDivisions [ [pure 0, pure 1] ]
                 , GridLayout 2 1 [(GridRange 0 1 0 1, 0), (GridRange 1 2 0 1, 1)] )
     , testCase "Singletons, vertical"
        $ pure 1
            ──
          pure 2
           =#?@= ( GridDivisions [ [pure 1]
                                 , [pure 2] ]
                 , GridLayout 1 2 [(GridRange 0 1 0 1, 1), (GridRange 0 1 1 2, 2)] )
     , testCase "Quadrat"
        $ pure 0 │ pure 1
           ──
          pure 2 │ pure 3
           =#?@= ( GridDivisions [ [pure 0, pure 1]
                                 , [pure 2, pure 3] ]
                 , GridLayout 2 2 [ (GridRange 0 1 0 1, 0), (GridRange 1 2 0 1, 1)
                                  , (GridRange 0 1 1 2, 2), (GridRange 1 2 1 2, 3) ] )
     , testCase "Rectangle, landscape"
        $ pure 0 │ pure 1 │ pure 4
           ──
          pure 2 │ pure 3 │ pure 5
           =#?@= ( GridDivisions [ [pure 0, pure 1, pure 4]
                                 , [pure 2, pure 3, pure 5] ]
                 , GridLayout 3 2
                   [ (GridRange 0 1 0 1, 0), (GridRange 1 2 0 1, 1), (GridRange 2 3 0 1, 4)
                   , (GridRange 0 1 1 2, 2), (GridRange 1 2 1 2, 3), (GridRange 2 3 1 2, 5) ] )
     , testCase "Rectangle, portrait"
        $ pure 0 │ pure 1
           ──
          pure 2 │ pure 3
           ──
          pure 4 │ pure 5
           =#?@= ( GridDivisions [ [pure 0, pure 1]
                                 , [pure 2, pure 3]
                                 , [pure 4, pure 5] ]
                 , GridLayout 2 3
                   [ (GridRange 0 1 0 1, 0), (GridRange 1 2 0 1, 1)
                   , (GridRange 0 1 1 2, 2), (GridRange 1 2 1 2, 3)
                   , (GridRange 0 1 2 3, 4), (GridRange 1 2 2 3, 5) ] )
     , testCase "Rectangle (built non-sequentially)"
        $ pure 0 │ pure 1
           ──
          pure 2 │ pure 3  ┃  pure 4
                               ──
                              pure 5
          @?= GridDivisions [ [pure 0, pure 1, pure 4]
                            , [pure 2, pure 3, pure 5] ]
     , testCase "Nonuniform last row"
        $ pure 0 │ pure 1 │ pure 2
           ──
          pure 3 │ pure 4 │ pure 5
           ──
          pure 6     │      pure 7
           =#?@= ( GridDivisions [ [GridDivisions [[pure 0,pure 1,pure 2]]]
                                 , [GridDivisions [[pure 3,pure 4,pure 5]]]
                                 , [GridDivisions [[pure 6   ,    pure 7]]] ]
                 , GridLayout 4 3
                   [ (GridRange 0 1 0 1, 0), (GridRange 1 3 0 1, 1), (GridRange 3 4 0 1, 2)
                   , (GridRange 0 1 1 2, 3), (GridRange 1 3 1 2, 4), (GridRange 3 4 1 2, 5)
                   ,        (GridRange 0 2 2 3, 6),         (GridRange 2 4 2 3, 7)          ] )
     , testCase "Nonuniform first row"
        $ pure 0 │ pure 1 │ pure 2
           ──
          pure 3     │      pure 5
           ──
          pure 6     │      pure 7
           =#?@= (GridDivisions [[GridDivisions [[pure 0,pure 1,pure 2]]]
                                ,[GridDivisions [[pure 3   ,    pure 5]
                                                ,[pure 6   ,    pure 7]]]]
                 , GridLayout 4 3
                   [ (GridRange 0 1 0 1, 0), (GridRange 1 3 0 1, 1), (GridRange 3 4 0 1, 2)
                   ,        (GridRange 0 2 1 2, 3),         (GridRange 2 4 1 2, 5)
                   ,        (GridRange 0 2 2 3, 6),         (GridRange 2 4 2 3, 7)          ] )
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


infix 1 =#?@=
(=#?@=) :: (Eq a, Show a) => Gridded a -> (Gridded a, GridLayout a) -> Assertion
a =#?@= b = (a, layoutGrid a) @?= b
