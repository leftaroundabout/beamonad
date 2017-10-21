{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Text.Lucius
import Text.Hamlet
import Math.LaTeX.Prelude

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


   "Another slide"
    ======
    vconcat [ staticContent $ [hamlet|
                 Static text
                 <br>
                 More text
                 <br>
                 More text
               |]()
            , do "Click me!"
                 "You've clicked."
            , do "No, me!"
                 "You've clicked."
            ]


   "A slide with grid layout"
    ====== do
     "slide"%##[["lside","rtop"]
               ,["lside","rbot"]]
        $  "lside"#%filling 8 "This goes on the left side"
        <> "rtop"#%filling 6 "This goes right on top"
        <> "rbot"#%filling 6 "This goes right down"
   
   
   t <- serverSide getCurrentTime
   "The current time at the server"
    ====== do
     fromString $ show t


   "A slide with automatic grid"
    ====== do
      "bla" │ "bli" │ "blo"
        ──
       "blum"   │  "blubb"


   ()<-"Hydra"
    ======
    fix (\h -> "head" >>= \() -> h │ h
                                 ──
                                 h │ h )


   "Some maths"
    ======
    "Let's consider "<> 𝑎*𝑏/(7-pi) $<>"..."




filling :: Int -> String -> Presentation
filling n = fromString . concat . replicate n . (++" ")
