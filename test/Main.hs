{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Presentation.Yeamer
import Text.Lucius

main :: IO ()
main = yeamer $
   Styling
     ([lucius|body {background-color: black}|] ())
     "Simple test “presentation”"
