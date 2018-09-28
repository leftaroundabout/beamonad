-- |
-- Module      : Presentation.Yeamer.Maths
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
-- Convenience module, re-exporting the necessary LaTeX builders for writing maths
-- in a Yeamer presentation.

{-# LANGUAGE CPP #-}

module Presentation.Yeamer.Maths
                ( module Presentation.Yeamer
                , module Math.LaTeX.Prelude
                , module Math.LaTeX.StringLiterals
                , Math
                ) where


import Presentation.Yeamer
import Math.LaTeX.Prelude hiding ( maths
#if MIN_VERSION_TeX_my_math(0,201,2)
                                 , ($<>)
#endif
                                 )
import Math.LaTeX.StringLiterals ()
import Text.LaTeX (LaTeX)

type Math = Expression LaTeX
