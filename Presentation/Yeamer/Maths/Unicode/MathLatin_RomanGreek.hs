-- |
-- Module      : Presentation.Yeamer.Maths.Unicode.MathLatin_RomanGreek
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
-- Convenience module, re-exporting the necessary LaTeX builders for writing maths
-- in a Yeamer presentation.

{-# LANGUAGE CPP            #-}
{-# LANGUAGE TypeFamilies   #-}

module Presentation.Yeamer.Maths.Unicode.MathLatin_RomanGreek
                ( module Presentation.Yeamer
                , module Math.LaTeX.Prelude
                , module Math.LaTeX.StringLiterals
                , Presentation.Yeamer.Maths.Unicode.MathLatin_RomanGreek.maths
                , (Presentation.Yeamer.Maths.Unicode.MathLatin_RomanGreek.$<>)
                , Math
                ) where


import Presentation.Yeamer hiding (($<>), maths)
import qualified Presentation.Yeamer.Maths as YMth
import Math.LaTeX.Prelude hiding ( maths
#if MIN_VERSION_TeX_my_math(0,201,2)
                                 , ($<>)
#endif
                                 )
import CAS.Dumb (CAS, Encapsulation, Infix)
import CAS.Dumb.Symbols.Unicode.MathLatin_RomanGreek__BopomofoGaps
           as UnicodeMth
import Math.LaTeX.StringLiterals ()
import Text.LaTeX (LaTeX)

type Math = Expression LaTeX

($<>) :: CAS (Infix LaTeX) (Encapsulation LaTeX) (UnicodeMth.Symbol LaTeX)
              -> Presentation -> Presentation
($<>) = (YMth.$<>)

maths :: r ~ ()
        => [[CAS (Infix LaTeX) (Encapsulation LaTeX) (UnicodeMth.Symbol LaTeX)]]
          -> String -> IPresentation m r
maths = YMth.maths

