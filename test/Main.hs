{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Text.Lucius
import Text.Hamlet
import Data.Foldable
import Data.Semigroup
import Data.String (fromString)

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
                 |] ()) $
   sequential
     [ addHeading "Heading"
        $ "Simple test “presentation”"
     , addHeading "Another slide"
        $ vconcat
            [ staticContent $ [hamlet|
                 Static text
                 <br>
                 More text
                 <br>
                 More text
               |]()
            , sequential
                [ "Click me!"
                , "You've clicked." ]
            , sequential
                [ "No, me!"
                , "You've clicked." ]
            ]
     , addHeading "A slide with grid layout"
        $ "slide"%##[["lside","rtop"]
                    ,["lside","rbot"]]
        $  "lside"#%filling 8 "This goes on the left side"
        <> "rtop"#%filling 6 "This goes right on top"
        <> "rbot"#%filling 6 "This goes right down"
     ]

filling :: Int -> String -> Presentation
filling n = fromString . concat . replicate n . (++" ")
