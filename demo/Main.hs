{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Presentation.Yeamer.Maths

import Text.Cassius

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
   div
     height: 100%
     text-align: center
     margin: auto
   .headed-container
     height: 80%
   .vertical-concatenation
     display: flex
     flex-direction: column
  |] ()
