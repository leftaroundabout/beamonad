-- |
-- Module      : Presentation.Yeamer
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE UnicodeSyntax          #-}
{-# LANGUAGE ConstraintKinds        #-}

module Presentation.Yeamer ( Presentation
                           , staticContent, serverSide
                           , divClass, (#%), (%##)
                           , addHeading, (======), vconcat
                           , styling
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
import Presentation.Yeamer.Internal.Grid

import Text.Cassius (Css)
import Text.Julius (rawJS)

import Data.Foldable (fold)
import qualified Data.Semigroup as SG
import Data.Semigroup.Numbered
import Data.Monoid
import Data.Maybe
import Data.Functor.Identity
import Control.Monad
import Control.Arrow (first, second)

import Data.Function ((&))

import GHC.Generics

type PrPath = Text
data PositionChange = PositionChange
    { posChangeLevel :: PrPath
    , posChangeIsRevert :: Bool
    } deriving (Generic)
instance JSON.FromJSON PositionChange

data Container t where
  WithHeading :: Html -> Container Identity
  ManualDivs :: Container (Map Text)
  GriddedBlocks :: Container Gridded
  CustomEncapsulation :: (t Html -> Html) -> Container t

type Sessionable r = (ToJSON r, FromJSON r)

data IPresentation m r where
   StaticContent :: Html -> IPresentation m ()
   Resultless :: IPresentation m r -> IPresentation m ()
   Styling :: [Css] -> IPresentation m r -> IPresentation m r
   Encaps :: (Traversable t, Sessionable r, Sessionable (t ()))
               => Container t -> t (IPresentation m r) -> IPresentation m (t r)
   Pure :: r -> IPresentation m r
   Deterministic :: (r -> s) -> IPresentation m r -> IPresentation m s
   Interactive :: Sessionable r
          => IPresentation m () -> m r -> IPresentation m r
   Dependent :: Sessionable x
                   => IPresentation m x -> (x -> IPresentation m r) -> IPresentation m r
instance (r ~ ()) => IsString (IPresentation m r) where
  fromString = StaticContent . fromString

type Presentation = IPresentation IO ()

mkYesod "Presentation" [parseRoutes|
/ HomeR GET
/changeposition ChPosR POST
/reset ResetR GET
|]
instance Yesod Presentation
instance YesodJquery Presentation


preprocPres :: IPresentation m r -> IPresentation m r
preprocPres (StaticContent c) = StaticContent c
preprocPres (Resultless p) = Resultless $ preprocPres p
preprocPres (Styling s p) = Styling s $ preprocPres p
preprocPres (Encaps (WithHeading h) p) = Encaps (WithHeading h) $ preprocPres<$>p
preprocPres (Encaps ManualDivs p) = Encaps ManualDivs $ preprocPres<$>p
preprocPres (Encaps (CustomEncapsulation f) p)
                = Encaps (CustomEncapsulation f) $ preprocPres<$>p
preprocPres (Encaps GriddedBlocks p)
           = Styling grids
           . divClass gridClass
           . fmap (backonstruct . map (first $ read . Txt.unpack) . Map.toList)
           . Encaps ManualDivs
           $ preprocPres <$> layouted
 where (GridLayout w h prelayed, backonstruct) = layoutGridP p
       layouted = Map.fromList $ first (("autogrid-range_"<>) . idc) . snd <$> prelayed
       gridRep :: [[Text]]
       gridRep = foldr fill (replicate h $ replicate w ".") prelayed
        where fill (GridRange xb xe yb ye, (i, _)) field
                 = yPre ++ [ xPre ++ (idc i<$xRel) ++ xPost
                           | xAll <- yRel
                           , let (xPre, (xRel, xPost)) = splitAt xb xAll
                                               & second (splitAt $ xe-xb) ] ++ yPost
               where (yPre, (yRel, yPost)) = splitAt yb field
                                   & second (splitAt $ ye-yb)
       idc i
        | c <- toEnum $ fromEnum 'a' + i
        , c <= 'z'                        = Txt.singleton c
       gridClass = "autogrid_"<>Txt.intercalate "-" (Txt.concat<$>gridRep)
       grids = [lucius|
                 div .#{gridClass} {
                    display: grid;
                    grid-template-areas: #{areas}
                 }
               |]()
             : [ [lucius| div .#{divid} { grid-area: #{ist} } |]()
               | (_, (i, _)) <- prelayed
               , let ist = idc i
                     divid = "autogrid-range_" <> ist ]
        where areas = fold ["\""<>Txt.intercalate " " line<>"\" "
                           | line <- gridRep ]

preprocPres (Pure x) = Pure x
preprocPres (Deterministic f p) = Deterministic f $ preprocPres p
preprocPres (Interactive p a) = Interactive (preprocPres p) a
preprocPres (Dependent d o) = Dependent (preprocPres d) (preprocPres<$>o)



getHomeR :: Handler Html
getHomeR = do
   presentation <- getYesod
   defaultLayout $ do
      addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
      slide <- chooseSlide "" "" Nothing Nothing presentation
      let contents = go 0 slide
      toWidget contents
 where chooseSlide :: PrPath -> Text -> Maybe PrPath -> Maybe PrPath
                       -> IPresentation m r -> WidgetT Presentation IO Presentation
       chooseSlide _ "" Nothing Nothing (StaticContent conts) = pure $ StaticContent conts
       chooseSlide path "" Nothing Nothing (Styling sty conts)
                     = mapM_ toWidget sty >> chooseSlide path "" Nothing Nothing conts
       chooseSlide path "" Nothing Nothing (Encaps ManualDivs conts)
           = discardResult . Encaps ManualDivs
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
       go :: Int -> IPresentation m r -> Html
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
       go lvl (Encaps ManualDivs conts)
           = go lvl $ Encaps (CustomEncapsulation $ \contsrs
                  -> foldMap (\(i,c) -> [hamlet| <div class=#{i}> #{c} |]() )
                      $ Map.toAscList contsrs
                 ) conts
       go lvl (Encaps (CustomEncapsulation f) conts) = f $ go lvl <$> conts
       go _ p = error $ outerConstructorName p <> " cannot be rendered."



instance (Monoid r, Sessionable r) => SG.Semigroup (IPresentation m r) where
  StaticContent c <> StaticContent d = StaticContent $ c<>d
  Encaps ManualDivs elems₀ <> Encaps ManualDivs elems₁
     = Encaps ManualDivs . goUnion elems₀ 0 $ Map.toList elems₁
   where goUnion :: Sessionable ρ
               => Map Text (IPresentation m ρ) -> Int -> [(Text,IPresentation m ρ)]
                      -> Map Text (IPresentation m ρ)
         goUnion settled _ []
                        = settled
         goUnion settled iAnonym ((k,e):es)
          | k `Map.notMember` settled
                        = goUnion (Map.insert k e settled) iAnonym es
          | k' <- "anonymousCell-"<>Txt.pack(show iAnonym)
          , k'`Map.notMember` settled
                        = goUnion (Map.insert k' (divClass k e) settled) iAnonym es
         goUnion s i es = goUnion s (i+1) es
  Resultless (Encaps ManualDivs ps) <> Resultless (Encaps ManualDivs qs)
      = Resultless $ Encaps ManualDivs (discardResult<$>ps)
               SG.<> Encaps ManualDivs (discardResult<$>qs)
  p <> q = fmap fold . Encaps ManualDivs $ Map.fromList
             [("anonymousCell-0", p), ("anonymousCell-1", q)]
instance ∀ m . Monoid (IPresentation m ()) where
  mappend = (SG.<>)
  mempty = Resultless $ Encaps ManualDivs (Map.empty :: Map Text (IPresentation m ()))

instance ∀ m . SemigroupNo 0 (IPresentation m ()) where
  sappendN _ (Resultless (Encaps GriddedBlocks l))
             (Resultless (Encaps GriddedBlocks r))
           = Resultless . Encaps GriddedBlocks $ (discardResult<$>l) │ (discardResult<$>r)
  sappendN _ l@(Resultless (Encaps GriddedBlocks _)) r
           = l │ Resultless (Encaps GriddedBlocks $ pure r)
  sappendN _ l r
           = Resultless (Encaps GriddedBlocks $ pure l) │ r

instance ∀ m . SemigroupNo 1 (IPresentation m ()) where
  sappendN _ (Resultless (Encaps GriddedBlocks t))
             (Resultless (Encaps GriddedBlocks b))
           = Resultless . Encaps GriddedBlocks $ (discardResult<$>t) ── (discardResult<$>b)
  sappendN _ t@(Resultless (Encaps GriddedBlocks _)) b
           = t │ Resultless (Encaps GriddedBlocks $ pure b)
  sappendN _ t b
           = Resultless (Encaps GriddedBlocks $ pure t) ── b


outerConstructorName :: IPresentation m r -> String
outerConstructorName (StaticContent _) = "StaticContent"
outerConstructorName (Styling _ _) = "Styling"
outerConstructorName (Encaps (WithHeading _) _) = "Encaps WithHeading"
outerConstructorName (Encaps (CustomEncapsulation _) _) = "Encaps CustomEncapsulation"
outerConstructorName (Encaps ManualDivs _) = "Encaps ManualDivs"
outerConstructorName (Encaps GriddedBlocks _) = "Encaps GriddedBlocks"
outerConstructorName (Pure _) = "Pure"
outerConstructorName (Deterministic _ _) = "Deterministic"
outerConstructorName (Interactive _ _) = "Interactive"
outerConstructorName (Dependent _ _) = "Dependent"

discardResult :: IPresentation m r -> IPresentation m ()
discardResult (StaticContent c) = StaticContent c
discardResult (Resultless p) = Resultless p
discardResult p = Resultless p

serverSide :: Sessionable a => m a -> IPresentation m a
serverSide = Interactive (pure ())

instance Functor (IPresentation m) where
  fmap f (Deterministic g q) = Deterministic (f . g) q
  fmap f (Pure x) = Pure $ f x
  fmap f q = Deterministic f q
instance Applicative (IPresentation m) where
  pure = Pure
instance Monad (IPresentation m) where
  return = pure
  StaticContent c >>= f = Dependent (StaticContent c) f
  Resultless p >>= f = Dependent (Resultless p) f
  Styling s p >>= f = case p >>= f of
     Dependent p' f' -> Dependent (Styling s p') f'
  Encaps (WithHeading h) (Identity p) >>= f = case p >>= f . Identity of
     Dependent p' f' -> Dependent (Encaps (WithHeading h) $ Identity p') $ f' . runIdentity
  Encaps ManualDivs ps >>= f = Dependent (Encaps ManualDivs ps) f
  Pure x >>= f = f x
  Deterministic g p >>= f = p >>= f . g
  Interactive p q >>= f = Dependent (Interactive p q) f
  Dependent p g >>= f = Dependent p $ g >=> f
  o >> Interactive (Pure _) q = Interactive (discardResult o) q
  o >> n = o >>= const n
    

infixr 6 ======
-- | Infix synonym of 'addHeading'. Intended to be used
--   in @do@ blocks, for headings of presentation slides.
(======) :: Sessionable r => Html -> IPresentation m r -> IPresentation m r
(======) = addHeading

addHeading :: Sessionable r => Html -> IPresentation m r -> IPresentation m r
addHeading h = fmap runIdentity . Encaps (WithHeading h) . Identity

divClass :: Sessionable r => Text -> IPresentation m r -> IPresentation m r
divClass cn = fmap (Map.!cn) . Encaps ManualDivs . Map.singleton cn

-- | Make a CSS grid, with layout as given in the names matrix.
infix 9 %##
(%##) :: Sessionable r => Text -> [[Text]] -> IPresentation m r -> IPresentation m r
cn %## grid = divClass cn . Styling [[lucius|
           div .#{cn} {
             display: grid;
             grid-template-areas: #{areas};
             grid-area: #{cn};
           }
        |]()]
 where areas = fold ["\""<>Txt.intercalate " " line<>"\" "
                    | line <- grid ]
 
infix 8 #%
-- | Make this a named grid area.
(#%) :: Sessionable r => Text -> IPresentation m r -> IPresentation m r
areaName#%Encaps ManualDivs dns
 | [(cn,q)]<-Map.toList dns   = Encaps ManualDivs . Map.singleton cn $ Styling [[lucius|
           div .#{cn} {
              grid-area: #{areaName}
           }
       |]()] q
areaName#%c = fmap (Map.!areaName) $ areaName
                #% Encaps ManualDivs (Map.singleton areaName c)

styling :: Css -> IPresentation m r -> IPresentation m r
styling = Styling . pure

staticContent :: Monoid r => Html -> IPresentation m r
staticContent = fmap (const mempty) . StaticContent

vconcat :: (Monoid r, Sessionable r) => [IPresentation m r] -> IPresentation m r
vconcat l = fmap (\rs -> fold [rs Map.! i | i<-indices])
             . divClass "vertical-concatenation" . Encaps ManualDivs
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
        let go, go' :: (PrPath, Text) -> [String] -> IPresentation IO r -> Handler (Maybe r)
            go _ [] (StaticContent _) = return $ Just ()
            go _ [] (Pure x) = return $ Just x
            go _ [] (Interactive _ q) = Just <$> liftIO q
            go crumbs path (Encaps (WithHeading _) (Identity cont))
                = fmap Identity <$> go crumbs path cont
            go (crumbh,crumbp) [] (Encaps ManualDivs conts)
                = sequence <$> Map.traverseWithKey
                     (\divid -> go (crumbh<>" div."<>divid, crumbp) []) conts
            go crumbs path (Deterministic f c) = fmap f <$> go crumbs path c
            go _ [] (Resultless c) = return $ Just ()
            go crumbs path (Resultless c) = const(Just()) <$> go crumbs path c
            go crumbs path (Interactive p _) = const Nothing <$> go crumbs path p
            go (crumbh, crumbp) (('0':prog):path') (Dependent def _)
                = const Nothing <$> go' (crumbh, crumbp<>"0") (prog:path') def
            go (crumbh, crumbp) (('1':prog):path') (Dependent def opt) = do
               key <- lookupProgress $ crumbh <> " div.no"<>crumbp<>"slide"
               case key of
                 Just k -> go' (crumbh, crumbp<>"1") (prog:path') $ opt k
                 Nothing -> do
                   Just k <- go' (crumbh, crumbp<>"0") [] def
                   go' (crumbh, crumbp<>"1") (prog:path') $ opt k
            go (crumbh,crumbp) [[]] (Dependent def opt) = do
               key <- go' (crumbh,crumbp<>"0") [[]] def
               case key of
                 Just k -> do
                   setProgress path Nothing k
                   skipContentless (crumbh, crumbp<>"1") $ opt k
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
            go (crumbh, _) (divid:path) (Encaps ManualDivs conts)
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
            skipContentless :: (PrPath, Text) -> IPresentation IO r -> Handler (Maybe r)
            skipContentless _ (Pure x) = return $ Just x
            skipContentless crumbs (Interactive p a) = do
               ll <- skipContentless crumbs p
               case ll of
                 Just _ -> Just <$> liftIO a
                 Nothing -> return Nothing
            skipContentless (crumbh,crumbp) (Dependent def opt) = do
               let thisDecision = crumbh <> " div.no"<>crumbp<>"slide"
               key <- lookupProgress thisDecision
               case key of
                 Just k -> skipContentless (crumbh, crumbp<>"1") $ opt k
                 Nothing -> do
                    key' <- skipContentless (crumbh, crumbp<>"0") def
                    case key' of
                      Just k' -> do
                        setProgress thisDecision (Just path) k'
                        skipContentless (crumbh, crumbp<>"1") $ opt k'
                      Nothing -> return Nothing
            skipContentless crumbs (Styling _ c) = skipContentless crumbs c
            skipContentless crumbs (Deterministic f c)
                = fmap f <$> skipContentless crumbs c
            skipContentless _ (StaticContent _) = return Nothing
            skipContentless _ (Encaps _ _) = return Nothing
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
        | Just decoded <- JSON.decode (BSL.fromStrict bs) = decoded
        | otherwise = error $
            "Internal error in `lookupProgress`: value "++show bs++" cannot be decoded."


setProgress :: (MonadHandler m, JSON.ToJSON x) => PrPath -> Maybe PrPath -> x -> m ()
setProgress path skippedFrom prog = do
   setSessionBS ("progress"<>path) (BSL.toStrict $ JSON.encode prog)
   forM_ skippedFrom $ setSession ("progress-skip-origin"<>path)

revertProgress :: MonadHandler m => PrPath -> m ()
revertProgress path = do
   deleteSession $ "progress"<>path
   let skipOrigKey = "progress-skip-origin"<>path
   lookupSession skipOrigKey >>= mapM_ `id` \skippedFrom -> do
        deleteSession $ "progress"<>skippedFrom
        deleteSession skipOrigKey

yeamer :: Presentation -> IO ()
yeamer = warp 14910 . preprocPres
