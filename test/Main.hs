{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Presentation.Yeamer.Maths

import Text.Lucius
import Text.Hamlet

import Data.Foldable
import Data.Semigroup
import Data.Semigroup.Numbered
import Data.String (fromString)
import Data.Function (fix)

import Data.Time.Clock

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
   
   
   t <- serverSide getCurrentTime
   "The current time at the server"
    ====== do
     fromString $ show t


   ()<-"Hydra"
    ======
    fix (\h -> "head" >>= \() -> h │ h
                                 ──
                                 h │ h )


   return ()




filling :: Int -> String -> Presentation
filling n = fromString . concat . replicate n . (++" ")
