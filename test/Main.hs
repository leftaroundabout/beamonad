{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Text.Lucius
import Text.Hamlet

main :: IO ()
main = yeamer . Styling ([lucius|
                  body { color: white
                       ; background-color: black } |] ()) $
   Encaps (\cont -> [hamlet|
              <h2>
                 Heading
              <div>
                 #{cont}|] ())
          "Simple test “presentation”"
