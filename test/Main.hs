{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Text.Lucius
import Text.Hamlet

main :: IO ()
main = yeamer . Styling ([lucius|
                  body { color: white
                       ; background-color: black } |] ()) $
   Sequential
     [ addHeading "Heading"
        $ "Simple test “presentation”"
     , addHeading "Another slide"
        $ "Still nothing interesting..."
     ]
