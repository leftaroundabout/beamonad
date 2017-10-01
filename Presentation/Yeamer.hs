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

module Presentation.Yeamer ( Presentation(..)
                           , yeamer ) where

import Yesod
import Yesod.Form.Jquery

import qualified Data.Text as Txt
import Data.Text (Text)
import Data.String (IsString (..))

import Text.Cassius (Css)

import Data.Monoid

import GHC.Generics


data Presentation
    = StaticContent Html
    | Styling Css Presentation
    | Encaps (Html->Html) Presentation
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
getHomeR = do
    presentation <- getYesod
    slide <- chooseSlide "progress" presentation
    let contents = go slide
    defaultLayout . addStyle slide $ toWidget contents
 where chooseSlide _ (StaticContent conts) = pure $ StaticContent conts
       chooseSlide path (Styling sty conts) = Styling sty <$> chooseSlide path conts
       chooseSlide path (Encaps f conts) = Encaps f <$> chooseSlide path conts
       chooseSlide path (Sequential seq) = do
          positionCh <- lookupSession path
          n <- case positionCh of
            Nothing -> do
              setSession path "0"
              return 0
            Just pos -> return . read $ Txt.unpack pos
          chooseSlide (path<>"_"<>Txt.pack(show n)) $ seq !! n
       go (StaticContent conts) = conts
       go (Styling sty conts) = go conts
       go (Encaps f conts) = f $ go conts
       addStyle (Styling sty conts) = (toWidget sty >>) . addStyle conts
       addStyle (Encaps _ conts) = addStyle conts
       addStyle (StaticContent _) = id

yeamer :: Presentation -> IO ()
yeamer = warp 14910
