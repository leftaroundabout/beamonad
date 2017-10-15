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
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ConstraintKinds     #-}

module Presentation.Yeamer ( Presentation
                           , staticContent
                           , divClass, (#%), (%##)
                           , addHeading, (======), vconcat
                           , styling
                           , sequential
                           , yeamer ) where

import Yesod
import Yesod.Form.Jquery

import qualified Data.Text as Txt
import Data.Text (Text)
import Data.String (IsString (..))
import qualified Data.ByteString.Lazy as BSL
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
import Data.Maybe
import Data.Functor.Identity
import Control.Monad

import GHC.Generics

type PrPath = Text
data PositionChange = PositionChange
    { posChangeLevel :: PrPath
    , posChangeIsRevert :: Bool
    } deriving (Generic)
instance JSON.FromJSON PositionChange

data Container t where
  WithHeading :: Html -> Container Identity
  Simultaneous :: Container (Map Text)
  CustomEncapsulation :: (t Html -> Html) -> Container t

type Sessionable r = (ToJSON r, FromJSON r)

data IPresentation r where
   StaticContent :: Html -> Presentation
   Resultless :: IPresentation r -> Presentation
   Styling :: Css -> IPresentation r -> IPresentation r
   Encaps :: (Traversable t, Sessionable r, Sessionable (t ()))
               => Container t -> t (IPresentation r) -> IPresentation (t r)
   Pure :: r -> IPresentation r
   Deterministic :: (r -> s) -> IPresentation r -> IPresentation s
   Interactive :: Sessionable r
          => Presentation -> IO r -> IPresentation r
   Dependent :: Sessionable x
                   => IPresentation x -> (x -> IPresentation r) -> IPresentation r
instance (r ~ ()) => IsString (IPresentation r) where
  fromString = StaticContent . fromString

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
      slide <- chooseSlide "" "" Nothing Nothing presentation
      let contents = go 0 slide
      toWidget contents
 where chooseSlide :: PrPath -> Text -> Maybe PrPath -> Maybe PrPath
                       -> IPresentation r -> WidgetT Presentation IO Presentation
       chooseSlide _ "" Nothing Nothing (StaticContent conts) = pure $ StaticContent conts
       chooseSlide path "" Nothing Nothing (Styling sty conts)
                     = toWidget sty >> chooseSlide path "" Nothing Nothing conts
       chooseSlide path "" Nothing Nothing (Encaps Simultaneous conts)
           = discardResult . Encaps Simultaneous
               <$> (`Map.traverseWithKey`conts) `id` \i cell ->
                 chooseSlide (path<>" div."<>i) "" Nothing Nothing cell
       chooseSlide path "" Nothing Nothing (Encaps f conts)
           = discardResult . Encaps f
               <$> traverse (chooseSlide path "" Nothing Nothing) conts
       chooseSlide path "" Nothing Nothing (Interactive conts _)
           = discardResult <$> chooseSlide path "" Nothing Nothing conts
       chooseSlide path "" Nothing Nothing (Resultless conts)
           = discardResult <$> chooseSlide path "" Nothing Nothing conts
       chooseSlide path "" Nothing Nothing (Deterministic _ conts)
           = discardResult <$> chooseSlide path "" Nothing Nothing conts
       chooseSlide path pdiv bwd fwd (Dependent def opt) = do
          let progPath = path<>" div.no"<>pdiv<>"slide"
          positionCh <- lookupProgress progPath
          case positionCh of
            Nothing -> chooseSlide path (pdiv<>"0") bwd (Just progPath) def
            Just x -> chooseSlide path (pdiv<>"1") (Just progPath) fwd $ opt x
       chooseSlide path pdiv bwd fwd pres
        | isJust bwd || isJust fwd  = do
          let thisChoice = "no"<>pdiv<>"slide"
              newPath = (path<>" div."<>thisChoice)
              [revertPossible, progressPossible]
                 = maybe "false" (const "true") <$> [bwd,fwd] :: [Text]
              [previous,next] = maybe "null" (("'"<>).(<>"'")) <$> [bwd, fwd]
          toWidget [julius|
                 $("#{rawJS newPath}").click(function(e){
                     if (e.ctrlKey && #{rawJS revertPossible}) {
                         isRevert = true;
                         path = #{rawJS previous};
                     } else if (!(e.ctrlKey) && #{rawJS progressPossible}) {
                         isRevert = false;
                         path = #{rawJS next};
                     } else {
                         return;
                     }
                     e.stopPropagation();
                     $.ajax({
                           contentType: "application/json",
                           processData: false,
                           url: "@{ChPosR}",
                           type: "POST",
                           data: JSON.stringify({
                                   posChangeLevel: path,
                                   posChangeIsRevert: isRevert
                                 }),
                           dataType: "text"
                        });
                     setTimeout(function() {location.reload();}, 50);
                 })
               |]
          divClass thisChoice <$> chooseSlide newPath "" Nothing Nothing pres
       go :: Int -> IPresentation r -> Html
       go _ (StaticContent conts) = conts
       go _ (Pure _) = error $ "Error: impossible to render a slide of an empty presentation."
       go _ (Dependent _ _) = error $ "Internal error: un-selected Dependent option while rendering to HTML."
       go lvl (Deterministic _ conts) = go lvl conts
       go lvl (Resultless conts) = go lvl conts
       go lvl (Interactive conts _) = go lvl conts
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



instance (Monoid r, Sessionable r) => SG.Semigroup (IPresentation r) where
  StaticContent c <> StaticContent d = StaticContent $ c<>d
  Encaps Simultaneous elems₀ <> Encaps Simultaneous elems₁
     = Encaps Simultaneous . goUnion elems₀ 0 $ Map.toList elems₁
   where goUnion :: Sessionable ρ
               => Map Text (IPresentation ρ) -> Int -> [(Text,IPresentation ρ)]
                      -> Map Text (IPresentation ρ)
         goUnion settled _ []
                        = settled
         goUnion settled iAnonym ((k,e):es)
          | k `Map.notMember` settled
                        = goUnion (Map.insert k e settled) iAnonym es
          | k' <- "anonymousCell-"<>Txt.pack(show iAnonym)
          , k'`Map.notMember` settled
                        = goUnion (Map.insert k' (divClass k e) settled) iAnonym es
         goUnion s i es = goUnion s (i+1) es
  Resultless (Encaps Simultaneous ps) <> Resultless (Encaps Simultaneous qs)
      = Resultless $ Encaps Simultaneous (discardResult<$>ps)
               SG.<> Encaps Simultaneous (discardResult<$>qs)
  p <> q = fmap fold . Encaps Simultaneous $ Map.fromList
             [("anonymousCell-0", p), ("anonymousCell-1", q)]
instance Monoid Presentation where
  mappend = (SG.<>)
  mempty = Resultless $ Encaps Simultaneous (Map.empty :: Map Text Presentation)



outerConstructorName :: IPresentation r -> String
outerConstructorName (StaticContent _) = "StaticContent"
outerConstructorName (Styling _ _) = "Styling"
outerConstructorName (Encaps (WithHeading _) _) = "Encaps WithHeading"
outerConstructorName (Encaps (CustomEncapsulation _) _) = "Encaps CustomEncapsulation"
outerConstructorName (Encaps Simultaneous _) = "Encaps Simultaneous"
outerConstructorName (Pure _) = "Pure"
outerConstructorName (Deterministic _ _) = "Deterministic"
outerConstructorName (Interactive _ _) = "Interactive"
outerConstructorName (Dependent _ _) = "Dependent"

discardResult :: IPresentation r -> Presentation
discardResult (StaticContent c) = StaticContent c
discardResult (Resultless p) = Resultless p
discardResult p = Resultless p

instance Functor IPresentation where
  fmap f (Deterministic g q) = Deterministic (f . g) q
  fmap f (Pure x) = Pure $ f x
  fmap f q = Deterministic f q
instance Applicative IPresentation where
  pure = Pure
instance Monad IPresentation where
  return = pure
  StaticContent c >>= f = Dependent (StaticContent c) f
  Resultless p >>= f = Dependent (Resultless p) f
  Styling s p >>= f = case p >>= f of
     Dependent p' f' -> Dependent (Styling s p') f'
  Encaps (WithHeading h) (Identity p) >>= f = case p >>= f . Identity of
     Dependent p' f' -> Dependent (Encaps (WithHeading h) $ Identity p') $ f' . runIdentity
  Encaps Simultaneous ps >>= f = Dependent (Encaps Simultaneous ps) f
  Pure x >>= f = f x
  Deterministic g p >>= f = p >>= f . g
  Interactive p q >>= f = Dependent (Interactive p q) f
  Dependent p g >>= f = Dependent p $ g >=> f
    

infixr 0 ======
-- | Infix synonym of 'addHeading', with low fixity. Intended to be used
--   in @do@ blocks, for headings of presentation slides.
(======) :: Sessionable r => Html -> IPresentation r -> IPresentation r
(======) = addHeading

addHeading :: Sessionable r => Html -> IPresentation r -> IPresentation r
addHeading h = fmap runIdentity . Encaps (WithHeading h) . Identity

divClass :: Sessionable r => Text -> IPresentation r -> IPresentation r
divClass cn = fmap (Map.!cn) . Encaps Simultaneous . Map.singleton cn

-- | Make a CSS grid, with layout as given in the names matrix.
infix 9 %##
(%##) :: Sessionable r => Text -> [[Text]] -> IPresentation r -> IPresentation r
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
(#%) :: Sessionable r => Text -> IPresentation r -> IPresentation r
areaName#%Encaps Simultaneous dns
 | [(cn,q)]<-Map.toList dns   = Encaps Simultaneous . Map.singleton cn $ Styling ([lucius|
           div .#{cn} {
              grid-area: #{areaName}
           }
       |]()) q
areaName#%c = fmap (Map.!areaName) $ areaName
                #% Encaps Simultaneous (Map.singleton areaName c)

styling :: Css -> IPresentation r -> IPresentation r
styling = Styling

staticContent :: Monoid r => Html -> IPresentation r
staticContent = fmap (const mempty) . StaticContent

sequential :: Monoid r => [Presentation] -> IPresentation r
sequential [] = pure mempty
sequential [slide] = fmap (const mempty) slide
sequential (slide:slides) = Dependent slide . const $ sequential slides

vconcat :: (Monoid r, Sessionable r) => [IPresentation r] -> IPresentation r
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
    PositionChange path isRevert <- requireJsonBody
    if isRevert
     then revertProgress path
     else do
        let go, go' :: (PrPath, Text) -> [String] -> IPresentation r -> Handler (Maybe r)
            go _ [] (StaticContent _) = return $ Just ()
            go _ [] (Pure x) = return $ Just x
            go _ [] (Interactive _ q) = Just <$> liftIO q
            go crumbs path (Encaps (WithHeading _) (Identity cont))
                = fmap Identity <$> go crumbs path cont
            go (crumbh,crumbp) [] (Encaps Simultaneous conts)
                = sequence <$> Map.traverseWithKey
                     (\divid -> go (crumbh<>" div."<>divid, crumbp) []) conts
            go crumbs path (Deterministic f c) = fmap f <$> go crumbs path c
            go _ [] (Resultless c) = return $ Just ()
            go crumbs path (Resultless c) = const(Just()) <$> go crumbs path c
            go crumbs path (Interactive p _) = const Nothing <$> go crumbs path p
            go (crumbh, crumbp) (('0':prog):path') (Dependent def _)
                = const Nothing <$> go' (crumbh, crumbp<>"0") (prog:path') def
            go (crumbh, crumbp) (('1':prog):path') (Dependent _ opt) = do
               Just key <- lookupProgress $ crumbh <> " div.no"<>crumbp<>"slide"
               go' (crumbh, crumbp<>"1") (prog:path') $ opt key
            go (crumbh,crumbp) [[]] (Dependent def opt) = do
               key <- go' (crumbh,crumbp<>"0") [[]] def
               case key of
                 Just k -> do
                   setProgress path k
                   return Nothing
                 Nothing -> error $ outerConstructorName def ++ " refuses to yield a result value."
            go (crumbh, crumbp) [] (Dependent _ opt) = do
               key <- lookupProgress $ crumbh <> " div.no"<>crumbp<>"slide"
               case key of
                 Just k -> go' (crumbh, crumbp<>"1") [] $ opt k
                 Nothing -> return Nothing
            go _ (dir:_) (Dependent _ _)
               = error $ "Div-ID "++dir++" not suitable for making a Dependent choice."
            go crumbs path (Styling _ cont) = go crumbs path cont
            go (crumbh, _) (divid:path) (Encaps Simultaneous conts)
              | Just dividt <- Txt.stripPrefix "div." $ Txt.pack divid
              , Just subSel <- Map.lookup dividt conts
                   = fmap (Map.singleton dividt) <$> go (crumbh, "") path subSel
            go _ [] pres
               = error $ "Need further path information to extract value from a "++outerConstructorName pres
            go _ (dir:_) pres
               = error $ "Cannot index ("++dir++") further into a "++outerConstructorName pres
            go' crumbs path p@(Dependent _ _)  = go crumbs path p
            go' (crumbh,crumbp) ([]:t) p = go (crumbh<>" div.no"<>crumbp<>"slide", "") t p
            go' (crumbh,crumbp) [] p
             | not $ Txt.null crumbp  = go (crumbh<>" div.no"<>crumbp<>"slide", "") [] p
            go' crumbs path p = go crumbs path p
        presentation <- getYesod
        go ("","") (finePath <$> Txt.words path) presentation
        return ()
 where finePath p
        | Just prog <- Txt.stripPrefix "div.no"
                     =<< Txt.stripSuffix "slide" p
           = Txt.unpack prog
        | otherwise  = Txt.unpack p

getResetR :: Handler Html
getResetR = do
    clearSession
    redirect HomeR

lookupProgress :: (MonadHandler m, JSON.FromJSON x) => PrPath -> m (Maybe x)
lookupProgress path = fmap decode <$> lookupSessionBS ("progress"<>path)
 where decode bs
        | Just decoded <- JSON.decode (BSL.fromStrict bs)  = decoded
        | otherwise = error $
            "Internal error in `lookupProgress`: value "++show bs++" cannot be decoded."


setProgress :: (MonadHandler m, JSON.ToJSON x) => PrPath -> x -> m ()
setProgress path prog = setSessionBS ("progress"<>path) (BSL.toStrict $ JSON.encode prog)

revertProgress :: MonadHandler m => PrPath -> m ()
revertProgress path = deleteSession ("progress"<>path)

yeamer :: Presentation -> IO ()
yeamer = warp 14910
