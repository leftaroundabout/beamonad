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



main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
 []


