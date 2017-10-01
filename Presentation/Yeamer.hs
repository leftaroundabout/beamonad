-- |
-- Module      : Presentation.Yeamer
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}

module Presentation.Yeamer where

import Yesod
import Yesod.Form.Jquery

import qualified Data.Text as Txt
import Data.Text (Text)
import Data.String (IsString (..))

import GHC.Generics


data Presentation
    = StaticContent Html
    | Sequential [Presentation]
 deriving (Generic)
instance IsString Presentation where
  fromString = StaticContent . fromString

mkYesod "Presentation" [parseRoutes|
/ HomeR GET
|]
instance Yesod Presentation
instance YesodJquery Presentation

getHomeR :: Handler Html
getHomeR = defaultLayout . go =<< getYesod
 where go (StaticContent conts) = toWidget conts

yeamer :: Presentation -> IO ()
yeamer = warp 63146
