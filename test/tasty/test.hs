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

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck (Arbitrary(..))

import Control.Monad


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
 []


instance (Arbitrary a) => Arbitrary (Gridded a) where
  arbitrary = do
    isLeaf <- arbitrary
    if isLeaf
      then GridRegion <$> arbitrary
      else GridDivisions <$> do
        [rowspec,colspec] <- replicateM 2 arbitrary
        forM rowspec $ \() -> forM colspec $ \() -> arbitrary
