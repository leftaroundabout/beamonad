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
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}

module Presentation.Yeamer ( Presentation
                           , staticContent
                           , divClass, (#%), (%##)
                           , addHeading, vconcat
                           , styling
                           , sequential
                           , yeamer ) where

import Yesod
import Yesod.Form.Jquery

import qualified Data.Text as Txt
import Data.Text (Text)
import Data.String (IsString (..))
import qualified Data.Aeson as JSON
import qualified Text.Blaze.Html5 as HTM
import qualified Text.Blaze.Html5.Attributes as HTM
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Cassius (Css)
import Text.Julius (rawJS)

import Data.Foldable (fold)
import qualified Data.Semigroup as SG
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
  Simultaneous :: Container (Map Text)
  CustomEncapsulation :: (t Html -> Html) -> Container t

data IPresentation r where
   StaticContent :: r -> Html -> IPresentation r
   Styling :: Css -> IPresentation r -> IPresentation r
   Encaps :: Traversable t => Container t -> t (IPresentation r) -> IPresentation (t r)
   Sequential :: [IPresentation r] -> IPresentation [r]
   DynamicCalculation :: (r -> s) -> IPresentation r -> IPresentation s
instance Monoid r => IsString (IPresentation r) where
  fromString = StaticContent mempty . fromString

type Presentation = IPresentation ()

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
 where chooseSlide :: PrPath -> IPresentation r -> WidgetT Presentation IO (IPresentation r)
       chooseSlide _ (StaticContent r conts) = pure $ StaticContent r conts
       chooseSlide path (Styling sty conts) = toWidget sty >> chooseSlide path conts
       chooseSlide path (Encaps Simultaneous conts)
           = Encaps Simultaneous <$> (`Map.traverseWithKey`conts) `id` \i cell ->
                 chooseSlide (path<>" div."<>i) cell
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
              [previous,next] = Txt.pack . show <$>
                 [ max 0 $ n-1
                 , if null slidesToGo then n else n+1 ]
          toWidget [julius|
                 $("#{rawJS newPath}").click(function(e){
                     if (e.ctrlKey) {
                         reqTarget = #{rawJS previous};
                     } else {
                         reqTarget = #{rawJS next};
                     }
                     e.stopPropagation();
                     $.ajax({
                           contentType: "application/json",
                           processData: false,
                           url: "@{ChPosR}",
                           type: "POST",
                           data: JSON.stringify({
                                   posChangeLevel: "#{rawJS path}",
                                   posChangeTarget: reqTarget
                                 }),
                           dataType: "text"
                        });
                     setTimeout(function() {location.reload();}, 50);
                 })
               |]
          fmap pure . divClass thisChoice <$> chooseSlide newPath thisSlide
       go :: Int -> IPresentation r -> Html
       go _ (StaticContent _ conts) = conts
       go lvl (Styling sty conts) = go lvl conts
       go lvl (Encaps (WithHeading h) conts)
           = let lvl' = min 6 $ lvl + 1
                 hh = [HTM.h1, HTM.h2, HTM.h3, HTM.h4, HTM.h5, HTM.h6]!!lvl
             in go lvl' $ Encaps (CustomEncapsulation $ \(Identity contsr)
                                    -> HTM.div HTM.! HTM.class_ "headed-container"
                                         $ hh h <> contsr
                                 ) conts
       go lvl (Encaps Simultaneous conts)
           = go lvl $ Encaps (CustomEncapsulation $ \contsrs
                  -> foldMap (\(i,c) -> [hamlet| <div class=#{i}> #{c} |]() )
                      $ Map.toAscList contsrs
                 ) conts
       go lvl (Encaps (CustomEncapsulation f) conts) = f $ go lvl <$> conts



instance SG.Semigroup r => SG.Semigroup (IPresentation r) where
  StaticContent rc c <> StaticContent rd d = StaticContent (rc SG.<>rd) (c<>d)
  Encaps Simultaneous elems₀ <> Encaps Simultaneous elems₁
     = Encaps Simultaneous . goUnion elems₀ (0::Int) $ Map.toList elems₁
   where goUnion settled _ []
                        = settled
         goUnion settled iAnonym ((k,e):es)
          | k `Map.notMember` settled
                        = goUnion (Map.insert k e settled) iAnonym es
          | k' <- "anonymousCell-"<>Txt.pack(show iAnonym)
          , k'`Map.notMember` settled
                        = goUnion (Map.insert k' (divClass k e) settled) iAnonym es
         goUnion s i es = goUnion s (i+1) es

instance Functor IPresentation where
  fmap f (DynamicCalculation g q) = DynamicCalculation (f . g) q
  fmap f q = DynamicCalculation f q

addHeading :: Html -> IPresentation r -> IPresentation r
addHeading h = fmap runIdentity . Encaps (WithHeading h) . Identity

divClass :: Text -> IPresentation r -> IPresentation r
divClass cn = fmap (Map.!cn) . Encaps Simultaneous . Map.singleton cn

-- | Make a CSS grid, with layout as given in the names matrix.
infix 9 %##
(%##) :: Text -> [[Text]] -> IPresentation r -> IPresentation r
cn %## grid = divClass cn . Styling ([lucius|
           div .#{cn} {
             display: grid;
             grid-template-areas: #{areas};
             grid-area: #{cn};
           }
        |]())
 where areas = fold ["\""<>Txt.intercalate " " line<>"\" "
                    | line <- grid ]
 
infix 8 #%
-- | Make this a named grid area.
(#%) :: Text -> IPresentation r -> IPresentation r
areaName#%DynamicCalculation f q = f <$> areaName #% q
areaName#%Encaps Simultaneous dns
 | [(cn,q)]<-Map.toList dns   = Encaps Simultaneous . Map.singleton cn $ Styling ([lucius|
           div .#{cn} {
              grid-area: #{areaName}
           }
       |]()) q
areaName#%c = areaName #% divClass areaName c

styling :: Css -> IPresentation r -> IPresentation r
styling = Styling

staticContent :: Monoid r => Html -> IPresentation r
staticContent = StaticContent mempty

sequential :: Monoid r => [IPresentation r] -> IPresentation r
sequential = fmap fold . Sequential

vconcat :: Monoid r => [IPresentation r] -> IPresentation r
vconcat l = fmap (\rs -> fold [rs Map.! i | i<-indices])
             . divClass "vertical-concatenation" . Encaps Simultaneous
             . Map.fromList
             $ zipWith (\i c -> ("vConcat-item"<>i, c)) indices l
 where indices = showIndex <$> [0 .. ll-1]
       showIndex i = Txt.pack $ replicate (lll - length si) '0' ++ si
        where si = show i
       ll = length l
       lll = length $ show ll

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
