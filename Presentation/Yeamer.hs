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
import qualified Data.Aeson as JSON

import Text.Cassius (Css)
import Text.Julius (rawJS)

import Data.Monoid

import GHC.Generics

type PrPath = Text
data PositionChange = PositionChange
    { posChangeLevel :: PrPath
    , posChangeTarget :: Int
    } deriving (Generic)
instance JSON.FromJSON PositionChange

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
/changeposition ChPosR POST
/reset ResetR GET
|]
instance Yesod Presentation
instance YesodJquery Presentation

getHomeR :: Handler Html
getHomeR = do
   presentation <- getYesod
   defaultLayout $ do
      addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
      slide <- chooseSlide "" presentation
      let contents = go slide
      addStyle slide $ toWidget contents
 where chooseSlide _ (StaticContent conts) = pure $ StaticContent conts
       chooseSlide path (Styling sty conts) = Styling sty <$> chooseSlide path conts
       chooseSlide path (Encaps f conts) = Encaps f <$> chooseSlide path conts
       chooseSlide path (Sequential seq) = do
          positionCh <- lookupProgress path
          n <- case positionCh of
            Nothing -> do
              setProgress path 0
              return 0
            Just pos -> return pos
          let thisChoice = "no"<>Txt.pack(show n)<>"slide"
              newPath = (path<>" div."<>thisChoice)
              next = Txt.pack . show $ n + 1
          toWidget [julius|
                 $("#{rawJS newPath}").click(function(e){
                     e.stopPropagation();
                     $.ajax({
                           contentType: "application/json",
                           processData: false,
                           url: "@{ChPosR}",
                           type: "POST",
                           data: JSON.stringify({
                                   posChangeLevel: "#{rawJS path}",
                                   posChangeTarget: #{rawJS next}
                                 }),
                           dataType: "text"
                        });
                     setTimeout(function() {location.reload();}, 50);
                 })
               |]
          Encaps (\conts -> [hamlet|
                    <div class=#{thisChoice}>
                      #{conts}
                 |]()) <$> chooseSlide newPath (seq !! n)
       go (StaticContent conts) = conts
       go (Styling sty conts) = go conts
       go (Encaps f conts) = f $ go conts
       addStyle (Styling sty conts) = (toWidget sty >>) . addStyle conts
       addStyle (Encaps _ conts) = addStyle conts
       addStyle (StaticContent _) = id

postChPosR :: Handler ()
postChPosR = do
    PositionChange path tgt <- requireJsonBody
    setProgress path tgt
    redirect HomeR

getResetR :: Handler Html
getResetR = do
    clearSession
    redirect HomeR

lookupProgress :: MonadHandler m => PrPath -> m (Maybe Int)
lookupProgress path = fmap (read . Txt.unpack) <$> lookupSession ("progress"<>path)

setProgress :: MonadHandler m => PrPath -> Int -> m ()
setProgress path prog = setSession ("progress"<>path) (Txt.pack $ show prog)

yeamer :: Presentation -> IO ()
yeamer = warp 14910
