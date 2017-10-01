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
     [ Encaps (\cont -> [hamlet|
                <h2>
                   Heading
                <div>
                   #{cont}|] ())
              "Simple test “presentation”"
     , Encaps (\cont -> [hamlet|
                <h2>
                   Another slide
                <div>
                   #{cont}|] ())
              "Still nothing interesting..."
     ]
