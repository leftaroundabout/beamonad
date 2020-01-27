{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Presentation.Yeamer.Maths

import Text.Cassius

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
      <>" you click "<>emph"here..."
     "...the viewer will switch to the next item in the containing "<>verb"do"
      <>"-block. Use "<>verb"ctrl+click"<>" to revert this."

   "Slide content Ⅰ"
    ====== do
     "Yeamer uses your web browser for rendering the slides. Therefore you can"
        <>" in principle insert any HTML code, e.g. though Hamlet quasi quotes."
       ──"In most cases, it is however recommended to simply use the "
        <>verb"OverloadedStrings"<>" extension so text can be written in ordinary"
        <>" Haskell string literals, and to combine elements with monoid operators."
     
     "(You don't need to worry about <special> characters &other; HTML pitfalls"
      <>" – anything you put in a string literal will appear as-is.)"

     "For the layout, we don't re-invent the wheel: CSS does that job fine."
      ──styling ([cassius|
              .reddened
                  color: #f88
              .caps
                  font-size: 85%
                  text-transform: uppercase |]())
         ("You can at any point add "<> "reddened"#%"rules" <>" for"
         <>"caps"#%"css"<>" styling. Usually it's best to do it just once"
         <>" globally, and define "<>emph"small helper functions"<>" for details.")

   "Slide composition"
    ====== do
     "There are three different ways in which "<>verb"Presentation"
                  <>" is a semigroup, allowing you to put together slides"
                  <>" out of content chunks:"
      ── do
      "The standard Monoid operator "<>verb"<>"<>" simply concatenates"
       <>" text elements, or else brings the content as close together as possible."
      "The stacking operator "<>verb"──"
       ──"puts one element on top"
       ──"of another."
      (verb"==="<>"is an ASCII alternative,")
       ==="it does the same thing."
       ━━"So does "<>verb"━━"<>", except it has lower precedence."
      "The siding operator "<>verb"│"<>"puts one element beside another."
       ┃ "Alternatives: "<>verb"|||"<>" and "<>verb"┃"<>"."
     
     "All of these operators can be used together," │ "in any nesting."
                    ──
       "This allows arranging elements on a slide in an almost “WYSiWYG-like” way."
      ┃
        "At the browser level, this is implemented with CSS3 grids, which"
            <>" automatically negotiate suitable width, height, and word-wrap"
            <>" for each cell."
   
   "Slide content Ⅱ"
    ====== do
     "Static images can be included with "<>verb"imageFromFile"<>"."
      ──
      imageFromFile "img/beamonad.svg"
     
     "Especially for mathematical or data plots, it can make sense to generate"
       <>" the images on-the-fly right from the Haskell code. You can use"
       <>" anything that's able to generate image files, most popular being the "
       <>verb"diagrams"<>" library."
      ── do
       imageFromDiagram (let line = Dia.fromVertices [0 Dia.^& 0, 1 Dia.^& 0.1]
                         in Dia.lc Dia.green . Dia.lwO 3 $ line Dia.=== line)
       "It is "<>emph"not"<>" necessary to come up with filenames for such images."
        <>small(" (Internally, "<>verb"yeamer"<>" will use automatic filenames based"
                <>" on the hashed image content.)")
     "Other content that can easily be included: videos, maths equations,"
       <>" step-by-step animations..."
      ──"You come up with the next idea!"
   
   return ()


imageFromDiagram :: Dia.Diagram Dia.Cairo -> Presentation
imageFromDiagram dia = imageFromFileSupplier "png"
           $ \tgtFile -> Dia.renderCairo tgtFile
                           (Dia.mkSizeSpec $ Just 400 Dia.^& Just 200) dia


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
     width: 95%
     height: 95%
     text-align: center
     margin: auto
   .headed-container
     height: 80%
   .vertical-concatenation
     display: flex
     flex-direction: column
   .emph
     font-style: italic
   .small
     font-size: 67%
   .verb
     display: inline-block
     font-size: 86%
     background-color: #227
     font-family: "Ubuntu Mono", "Droid Sans mono", "Courier New"

  |] ()
emph = ("emph"#%)
verb = ("verb"#%)
small = ("small"#%)
