{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Presentation.Yeamer.Maths

import Text.Lucius
import Text.Hamlet

import Data.Foldable
import Data.String (fromString)
import Data.Function (fix)

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Data.Flat (Flat(..))

import qualified Diagrams.Prelude as Dia
import qualified Diagrams.Backend.Cairo as Dia

import Data.Function

main :: IO ()
main = yeamer . styling ([lucius|
                  body {
                    height: 100vh;
                  }
                  body {
                    color: white;
                    background-color: black;
                    font-size: 160%;
                  }
                  div {
                    height: 100%;
                    text-align: center;
                  }
                  pre {
                    text-align: left;
                  }
                  .headed-container {
                    height: 80%;
                  }
                  .vertical-concatenation {
                    display: flex;
                    flex-direction: column;
                  }
                 |] ()) $ do


   "Heading"
    ======
    "Simple test “presentation”"


   "Code block"
    ====== [plaintext|
       bla
         blub
           bli
         blum
          |]
   
   "Lazy Haskell structures"
    ======
     display [0::Int .. 16]
      ──
     display [ [0..n]
             | n <- [0::Int .. ] ]

   "Some maths"
    ======
    "Let's consider "<> 𝑎*𝑏/(7-pi) $<>". This ensures"
     <> maths[[ 𝑥 + 𝑦 ⩵ (-2*ζ,99+𝑢)∫d ρ 𝑧, ""∀:𝑎⪢9 ]
             ,[ 𝑥 - 𝑦 ⩵ 2786126792567    , ""∀:𝑏⪢3 ]]"."
     <>"Furthermore, "<> ( 3⊂19 ⩵ ω∩set 𝑚 ) $<>" implies that"
     <> maths[[ 37◞∑"foo" ≥ 𝑦◞◝(3.79,"bla") ]]","
     <>"and therefore "<> (-τ) $<>"."


   "A slide with automatic grid layout"
    ====== do
      "bla" │ "bli" │ "blo"
        ──
       "blum"   │  "blubb"


   "Image files"
    ====== do
    imageFromDiagram (
        Dia.circle 1 & Dia.lc Dia.red
      )
     ──
     "Plain circle"
    imageFromFile "/usr/share/icons/HighContrast/256x256/apps/firefox.png"
     ──
     "(Firefox logo)"

   
   "Manual HTML, and cell-wise sequencing"
    ======
    staticContent ([hamlet|
                 <i>Static</i> text
                 <br>
                 More <b>text</b>
                 <br>
                 <em>More</em> text
               |]())
     ──
     do "Click me!"
        "You've clicked."
     ──
     do "No, me!"
        "You've clicked."

   
   "Styling of inline elements"
    ====== styling ([lucius| .fattened {font-weight: bold; font-style: italic} |]())`id`
    mconcat (zipWith ($)
              (cycle [id, ("fattened"#%)])
              (fromString<$>words"Click any of these words.") )
   
   
   "The current time at the server"
    ====== do
     t <- serverSide getCurrentTime
     fromString $ show t


   ()<-"Hydra"
    ======
    fix (\h -> "head" >>= \() -> h │ h
                                 ──
                                 h │ h )


   return ()


imageFromDiagram :: Dia.Diagram Dia.Cairo -> Presentation
imageFromDiagram dia = imageFromFileSupplier "png"
           $ \tgtFile -> Dia.renderCairo tgtFile
                           (Dia.mkSizeSpec $ Just 640 Dia.^& Just 480) dia

filling :: Int -> String -> Presentation
filling n = fromString . concat . replicate n . (++" ")

instance Flat UTCTime where
  decode = fmap (posixSecondsToUTCTime . realToFrac . (id::Double->Double)) decode
  encode = encode . (id::Double->Double) . realToFrac . utcTimeToPOSIXSeconds
  size = size . (id::Double->Double) . realToFrac . utcTimeToPOSIXSeconds
