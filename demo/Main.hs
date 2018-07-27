{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Presentation.Yeamer.Maths

import Text.Cassius

import Data.Semigroup
import Data.Semigroup.Numbered

import qualified Diagrams.Prelude as Dia
import qualified Diagrams.Backend.Cairo as Dia

main :: IO ()
main = yeamer . styling style $ do
   ""
    ──
    "main-title"#%"The Yeamer Presentation Engine"
    ──
    "Click anywhere to start demo"
   
   "Idea"
    ======
    "Beamonad/Yeamer is a Haskell eDSL for writing screen presentations."
     ── do
     "It is based on a free-monad like data structure, with monad "<>verb">>"
      <>" representing sequencing of slides in the presentation. Thus, if"
      <>" you click "<>emph"here"
     "...the viewer will switch to the next item in the containing "<>verb"do"
      <>"-block. Use "<>verb"ctrl+click"<>" to revert this."

   return ()


imageFromDiagram :: Dia.Diagram Dia.Cairo -> Presentation
imageFromDiagram dia = imageFromFileSupplier "png"
           $ \tgtFile -> Dia.renderCairo tgtFile
                           (Dia.mkSizeSpec $ Just 640 Dia.^& Just 480) dia


style = [cassius|
   body
     height: 100vh
     color: #fc8
     background-color: #205
     font-size: 6vmin
     font-family: "Linux libertine", "Times New Roman"
   .main-title
     font-size: 180%
   h1
     font-size: 150%
   div
     height: 100%
     text-align: center
     margin: auto
   .headed-container
     height: 80%
   .vertical-concatenation
     display: flex
     flex-direction: column
   .emph
     font-style: italic
   .verb
     display: inline-block
     font-size: 86%
     background-color: #227
     font-family: "Ubuntu Mono", "Droid Sans mono", "Courier New"

  |] ()
emph = ("emph"#%)
verb = ("verb"#%)
