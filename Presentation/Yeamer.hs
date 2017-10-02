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
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}

module Presentation.Yeamer ( Presentation(..)
                           , addHeading, vconcat
                           , yeamer ) where

import Yesod
import Yesod.Form.Jquery

import qualified Data.Text as Txt
import Data.Text (Text)
import Data.String (IsString (..))
import qualified Data.Aeson as JSON
import qualified Text.Blaze.Html5 as HTM
import qualified Text.Blaze.Html5.Attributes as HTM

import Text.Cassius (Css)
import Text.Julius (rawJS)

import Data.Foldable ()
import Data.Monoid
import Data.Functor.Identity
import Control.Monad

import GHC.Generics

type PrPath = Text
data PositionChange = PositionChange
    { posChangeLevel :: PrPath
    , posChangeTarget :: Int
    } deriving (Generic)
instance JSON.FromJSON PositionChange

data Container t where
  WithHeading :: Html -> Container Identity
  VConcated :: Container []
  CustomEncapsulation :: (t Html -> Html) -> Container t

data Presentation where
   StaticContent :: Html -> Presentation
   Styling :: Css -> Presentation -> Presentation
   Encaps :: Traversable t => (Container t) -> t Presentation -> Presentation
   Sequential :: [Presentation] -> Presentation
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
      let contents = go 0 slide
      toWidget contents
 where chooseSlide :: PrPath -> Presentation -> WidgetT Presentation IO Presentation
       chooseSlide _ (StaticContent conts) = pure $ StaticContent conts
       chooseSlide path (Styling sty conts) = toWidget sty >> chooseSlide path conts
       chooseSlide path (Encaps VConcated conts)
           = Encaps VConcated <$> forM (zip [0::Int ..] conts) `id` \(i,cell) ->
                 chooseSlide (path<>" div.group"<>Txt.pack(show i)) cell
       chooseSlide path (Encaps f conts)
           = Encaps f <$> traverse (chooseSlide path) conts
       chooseSlide path (Sequential seq) = do
          positionCh <- lookupProgress path
          n <- case positionCh of
            Nothing -> do
              setProgress path 0
              return 0
            Just pos -> return pos
          let thisChoice = "no"<>Txt.pack(show n)<>"slide"
              newPath = (path<>" div."<>thisChoice)
              thisSlide:slidesToGo = case splitAt n seq of
                    (_,t:g) -> t:g
                    (f,[])  -> [last f]
          when (not $ null slidesToGo) $
            let next = Txt.pack . show $ n + 1
            in toWidget [julius|
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
          Encaps (CustomEncapsulation $ \(Identity conts) -> [hamlet|
                    <div class=#{thisChoice}>
                      #{conts}
                 |]()) . Identity <$> chooseSlide newPath thisSlide
       go :: Int -> Presentation -> Html
       go _ (StaticContent conts) = conts
       go lvl (Styling sty conts) = go lvl conts
       go lvl (Encaps (WithHeading h) conts)
           = let lvl' = min 6 $ lvl + 1
                 hh = [HTM.h1, HTM.h2, HTM.h3, HTM.h4, HTM.h5, HTM.h6]!!lvl
             in go lvl' $ Encaps (CustomEncapsulation $ \(Identity contsr)
                                    -> HTM.div $ hh h <> contsr
                                 ) conts
       go lvl (Encaps VConcated conts)
           = go lvl $ Encaps (CustomEncapsulation $ \contsrs
                  -> foldMap (\(i,c) -> let groupName = "group"<>Txt.pack(show i)
                                        in [hamlet| <div class=#{groupName}>
                                                        #{c} |]() )
                      $ zip [0::Int ..] contsrs
                 ) conts
       go lvl (Encaps (CustomEncapsulation f) conts) = f $ go lvl <$> conts

addHeading :: Html -> Presentation -> Presentation
addHeading h = Encaps (WithHeading h) . Identity

vconcat :: [Presentation] -> Presentation
vconcat = Encaps VConcated

postChPosR :: Handler ()
postChPosR = do
    PositionChange path tgt <- requireJsonBody
    setProgress path tgt

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
