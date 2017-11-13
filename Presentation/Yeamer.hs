-- |
-- Module      : Presentation.Yeamer
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE CPP                    #-}
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
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE ConstraintKinds        #-}

module Presentation.Yeamer ( Presentation
                           -- * Running a presentation
                           , yeamer
                           -- * Primitives
                           , staticContent, serverSide
                           -- ** Maths
                           , ($<>), maths
                           -- ** Images
                           , imageFromFile
                           -- * Structure / composition
                           , addHeading, (======), discardResult
                           -- * CSS
                           , divClass, spanClass, (#%), styling
                           ) where

import Yesod
import Yesod.Form.Jquery

import qualified Data.Text as Txt
import qualified Data.Text.Lazy as Txt (toStrict)
import Data.Text (Text)
import Data.String (IsString (..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as JSON
import qualified Text.Blaze.Html5 as HTM
import qualified Text.Blaze.Html5.Attributes as HTM
import qualified Text.Blaze.Html.Renderer.Text as HTMText
import Data.Map (Map)
import qualified Data.Map as Map
import Presentation.Yeamer.Internal.Grid

import Text.Cassius (Css)
import Text.Julius (rawJS)

import Yesod.Static
import Yesod.EmbeddedStatic
import qualified Language.Javascript.JQuery as JQuery
import Language.Haskell.TH.Syntax (Exp(LitE), Lit(StringL), runIO)

import qualified CAS.Dumb.Symbols as TMM
import qualified CAS.Dumb.Tree as TMM
import qualified Math.LaTeX.Prelude as TMM
import Text.LaTeX (LaTeX)
import qualified Text.LaTeX as LaTeX
import qualified Text.TeXMath as MathML
import qualified Text.XML.Light as XML

import Data.Foldable (fold)
import Control.Monad.Trans.Writer.JSONable
import Data.These
import qualified Data.Semigroup as SG
import Data.Semigroup.Numbered
import Data.Monoid
import Data.Maybe
import Data.Functor.Identity
import Control.Monad
import Control.Arrow (first, second)
import Control.Applicative

import Data.Function ((&))
import Data.Tuple (swap)

import System.FilePath (takeFileName, takeExtension, dropExtension, (<.>), (</>))
import System.Directory ( doesPathExist, makeAbsolute
                        , createDirectoryIfMissing
#if MIN_VERSION_directory(1,3,1)
                        , createFileLink )
#endif
                        )
#if !MIN_VERSION_directory(1,3,1)
import System.Posix.Files (createSymbolicLink)
#endif

import GHC.Generics
import Lens.Micro
import Lens.Micro.TH
import Data.Bifunctor (bimap)

#if !MIN_VERSION_directory(1,3,1)
createFileLink = createSymbolicLink
#endif

type PrPath = Text
data PositionChange = PositionChange
    { posChangeLevel :: PrPath
    , posChangeIsRevert :: Bool
    } deriving (Generic)
instance JSON.FromJSON PositionChange

data Container t where
  WithHeading :: Html -> Container Identity
  ManualCSSClasses :: Container (WriterT HTMChunkK [])
  GriddedBlocks :: Container Gridded
  CustomEncapsulation :: (t Html -> Html) -> Container t

data HTMChunkK = HTMDiv {_hchunkCSSClass::Text} | HTMSpan {_hchunkCSSClass::Text}
          deriving (Generic, Eq, Ord)
instance JSON.FromJSON HTMChunkK
instance JSON.ToJSON HTMChunkK
makeLenses ''HTMChunkK

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

data PresentationServer = PresentationServer {
      presentationToShow :: Presentation
    , getStatic :: EmbeddedStatic
    , getPseudostatic :: Static
    }

mkEmbeddedStatic False "myStatic" . pure . embedFileAt "jquery.js" =<< runIO JQuery.file

pStatDir :: FilePath
pStatDir = ".pseudo-static-content"

mkYesod "PresentationServer" [parseRoutes|
/ HomeR GET
/changeposition ChPosR POST
/r StepBackR GET
/reset ResetR GET
/static StaticR EmbeddedStatic getStatic
/pseudostatic PStaticR Static getPseudostatic
|]
instance Yesod PresentationServer where
  addStaticContent = embedStaticContent getStatic StaticR Right
instance YesodJquery PresentationServer

preprocPres :: IPresentation m r -> IPresentation m r
preprocPres (StaticContent c) = StaticContent c
preprocPres (Resultless p) = Resultless $ preprocPres p
preprocPres (Styling s p) = Styling s $ preprocPres p
preprocPres (Encaps (WithHeading h) p) = Encaps (WithHeading h) $ preprocPres<$>p
preprocPres (Encaps ManualCSSClasses p) = Encaps ManualCSSClasses $ preprocPres<$>p
preprocPres (Encaps (CustomEncapsulation f) p)
                = Encaps (CustomEncapsulation f) $ preprocPres<$>p
preprocPres (Encaps GriddedBlocks p)
           = Styling grids
           . divClass gridClass
           . fmap (backonstruct . map (first (read . Txt.unpack . _hchunkCSSClass) . swap)
                      . runWriterT)
           . Encaps ManualCSSClasses
           $ preprocPres <$> layouted
 where (GridLayout w h prelayed, backonstruct) = layoutGridP p
       layouted = WriterT $ swap . first (HTMDiv . ("autogrid-range_"<>) . idc)
                      . snd <$> prelayed
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
                 div.#{gridClass} {
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


isInline :: IPresentation m a -> Bool
isInline (StaticContent _) = True
isInline (Encaps ManualCSSClasses (WriterT qs)) = all (\(_,i) -> case i of
                   HTMSpan _ -> True
                   HTMDiv _ -> False ) qs
isInline (Encaps _ _) = False
isInline (Styling _ q) = isInline q
isInline (Interactive q _) = isInline q
isInline (Resultless q) = isInline q
isInline (Dependent q _) = isInline q
isInline (Deterministic _ q) = isInline q
isInline (Pure _) = True


getHomeR :: Handler Html
getHomeR = do
   PresentationServer presentation _ _ <- getYesod
   defaultLayout $ do
      addScript $ StaticR jquery_js
      slideChoice <- chooseSlide "" defaultChoiceName "" Nothing Nothing presentation
      (`here`slideChoice) $ \slide -> do
          let contents = go 0 slide
          toWidget contents
      return ()
 where chooseSlide :: PrPath -> (Text->PrPath) -> Text -> Maybe PrPath -> Maybe PrPath
                       -> IPresentation IO r -> WidgetT PresentationServer IO
                                                (These Presentation r)
       chooseSlide _ _ "" Nothing Nothing (StaticContent conts)
           = pure $ These (StaticContent conts) ()
       chooseSlide path choiceName "" Nothing Nothing (Styling sty conts)
                     = mapM_ toWidget sty
                        >> chooseSlide path choiceName "" Nothing Nothing conts
       chooseSlide path choiceName "" Nothing Nothing (Encaps f conts)
                  = postGather <$> cellwise f
        where postGather sq = case sequence sq of
               That p's -> That p's
               This _   -> This . discardResult $ Encaps f
                                $ fmap (maybe mempty id . (^?here)) sq
               These _ p's -> (`These`p's) . discardResult $ Encaps f
                                $ fmap (maybe mempty id . (^?here)) sq
              cellwise ManualCSSClasses
                | WriterT contsL <- conts
                     = WriterT <$> (`traverse`contsL) `id` \(cell,i) ->
                 (,i) <$> chooseSlide (path<>case i of
                                 HTMDiv c -> " div."<>c
                                 HTMSpan c -> " span."<>c
                              ) choiceName "" Nothing Nothing cell
              cellwise _ = traverse (chooseSlide path choiceName "" Nothing Nothing) conts
       chooseSlide path choiceName "" Nothing Nothing (Interactive conts followAction) = do
           purity <- chooseSlide path choiceName "" Nothing Nothing conts
           case purity ^? here of
             Just pres -> pure . This $ discardResult pres
             Nothing   -> That <$> liftIO followAction
       chooseSlide path choiceName "" Nothing Nothing (Resultless conts) = do
           purity <- chooseSlide path choiceName "" Nothing Nothing conts
           case purity ^? here of
             Just pres -> pure . (`These`()) $ discardResult pres
             Nothing   -> pure $ That ()
       chooseSlide path choiceName "" Nothing Nothing (Deterministic f conts) = do
           purity <- chooseSlide path choiceName "" Nothing Nothing conts
           pure $ bimap discardResult f purity
       chooseSlide path choiceName pdiv bwd fwd (Dependent def opt) = do
          let progPath = path<>" span."<>choiceName pdiv
          positionCh <- lookupProgress progPath
          case positionCh of
            Nothing -> do
              purity <- chooseSlide path choiceName (pdiv<>"0") bwd (Just progPath) def
              case preferThis purity of
                 Left pres -> pure . This $ discardResult pres
                 Right x -> do
                   setProgress progPath bwd x
                   chooseSlide path choiceName (pdiv<>"1") (Just progPath) fwd $ opt x
            Just x -> chooseSlide path choiceName (pdiv<>"1") (Just progPath) fwd $ opt x
       chooseSlide path choiceName pdiv bwd fwd pres
        | isJust bwd || isJust fwd  = do
          let thisChoice = choiceName pdiv
              newPath = (path<>" span."<>thisChoice)
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
                     history.replaceState({}, "back", "@{StepBackR}");
                     history.pushState({}, "current", "@{HomeR}");
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
          (here %~ spanClass thisChoice)
                  <$> chooseSlide newPath (disambiguateChoiceName choiceName)
                           "" Nothing Nothing pres
       chooseSlide _ _ _ _ _ (Pure x) = pure $ That x
       chooseSlide _ _ _ _ _ pres
          = error $ "Cannot display "++outerConstructorName pres
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
       go lvl (Encaps ManualCSSClasses conts)
           = go lvl $ Encaps (CustomEncapsulation $ \(WriterT contsrs)
                  -> foldMap (\(q,i) -> case i of
                               HTMDiv c -> [hamlet| <div class=#{withSupclass c}> #{q} |]()
                               HTMSpan c -> [hamlet| <span class=#{withSupclass c}> #{q} |]())
                      $ contsrs
                 ) conts
        where withSupclass c
               | Just _ <- Txt.stripPrefix "autogrid_" c
                            = "autogrid "<>c
               | otherwise  = c
       go lvl (Encaps (CustomEncapsulation f) conts) = f $ go lvl <$> conts
       go _ p = error $ outerConstructorName p <> " cannot be rendered."


preferThis :: These a b -> Either a b
preferThis (This a) = Left a
preferThis (That b) = Right b
preferThis (These a _) = Left a

hchunkFor :: Text -> IPresentation m r -> HTMChunkK
hchunkFor t p | isInline p  = HTMSpan t
              | otherwise   = HTMDiv t

instance (Monoid r, Sessionable r) => SG.Semigroup (IPresentation m r) where
  StaticContent c <> StaticContent d = StaticContent $ c<>d
  Encaps ManualCSSClasses (WriterT elems₀) <> Encaps ManualCSSClasses (WriterT elems₁)
     = Encaps ManualCSSClasses . WriterT . disambiguate $ elems₀ ++ elems₁
   where disambiguate = go 0 Map.empty
          where go _ _ [] = []
                go i occupied ((q,c):qs)
                 | Txt.null (_hchunkCSSClass c) || c`Map.member`occupied
                    = let c' = c & hchunkCSSClass .~ Txt.pack ("anonymousCell-"++show i)
                      in go (i+1) occupied (( fmap (fst . head . runWriterT)
                                             . Encaps ManualCSSClasses $ WriterT [(q,c)]
                                            , c' ):qs)
                 | otherwise
                    = (q,c) : go (i+1) (Map.insert c () occupied) qs
  Resultless (Encaps ManualCSSClasses ps) <> Resultless (Encaps ManualCSSClasses qs)
      = Resultless $ Encaps ManualCSSClasses (discardResult<$>ps)
               SG.<> Encaps ManualCSSClasses (discardResult<$>qs)
  Resultless p@(Encaps ManualCSSClasses _) <> c
      = Resultless p <> Resultless (Encaps ManualCSSClasses $ WriterT [(c,hchunkFor""c)])
  c <> Resultless p@(Encaps ManualCSSClasses _)
      = Resultless (Encaps ManualCSSClasses $ WriterT [(c,hchunkFor""c)]) <> Resultless p
  p <> q = fmap fold . Encaps ManualCSSClasses $ WriterT
             [(p, hchunkFor"anonymousCell-0"p), (q, hchunkFor"anonymousCell-1"q)]
instance ∀ m . Monoid (IPresentation m ()) where
  mappend = (SG.<>)
  mempty = Resultless $ Encaps ManualCSSClasses
                (WriterT [] :: WriterT HTMChunkK [] (IPresentation m ()))

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
           = t ── Resultless (Encaps GriddedBlocks $ pure b)
  sappendN _ t b
           = Resultless (Encaps GriddedBlocks $ pure t) ── b


outerConstructorName :: IPresentation m r -> String
outerConstructorName (StaticContent _) = "StaticContent"
outerConstructorName (Resultless _) = "Resultless"
outerConstructorName (Styling _ _) = "Styling"
outerConstructorName (Encaps (WithHeading _) _) = "Encaps WithHeading"
outerConstructorName (Encaps (CustomEncapsulation _) _) = "Encaps CustomEncapsulation"
outerConstructorName (Encaps ManualCSSClasses _) = "Encaps ManualCSSClasses"
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
  Pure f <*> x = fmap f x
  f <*> Pure x = fmap ($ x) f
--Styling s (Styling s' f) <*> x
--Encaps :: (Traversable t, Sessionable r, Sessionable (t ()))
--            => Container t -> t (IPresentation m r) -> IPresentation m (t r)
--Deterministic :: (r -> s) -> IPresentation m r -> IPresentation m s
--Interactive :: Sessionable r
--       => IPresentation m () -> m r -> IPresentation m r
--Dependent :: Sessionable x
--                => IPresentation m x -> (x -> IPresentation m r) -> IPresentation m r
  -- f <*> x = ap f x
instance Monad (IPresentation m) where
  return = pure
  StaticContent c >>= f = Dependent (StaticContent c) f
  Resultless p >>= f = Dependent (Resultless p) f
  Styling _ (Pure x) >>= f = f x
  Styling s (StaticContent c) >>= f = Dependent (Styling s (StaticContent c)) f
  Styling s (Resultless c) >>= f = Dependent (Styling s (Resultless c)) f
  Styling s (Styling s' x) >>= f = Styling (s++s') x >>= f
  Styling s (Encaps (WithHeading h) x) >>= f
      = Dependent (Styling s (Encaps (WithHeading h) x)) f
  Styling s (Encaps ManualCSSClasses x) >>= f
      = Dependent (Styling s (Encaps ManualCSSClasses x)) f
  Styling s (Deterministic g x) >>= f = Styling s x >>= f . g
  Styling s (Interactive p o) >>= f = Dependent (Interactive (Styling s p) o) f
  Styling s (Dependent p g) >>= f = Dependent (Styling s p) $ Styling s . g >=> f
  Encaps (WithHeading h) p >>= f = Dependent (Encaps (WithHeading h) p) f
  Encaps ManualCSSClasses ps >>= f = Dependent (Encaps ManualCSSClasses ps) f
  Pure x >>= f = f x
  Deterministic g p >>= f = p >>= f . g
  Interactive p q >>= f = Dependent (Interactive p q) f
  Dependent p g >>= f = Dependent p $ g >=> f
  o >> Interactive (Pure _) q = Interactive (discardResult o) q
  o >> Pure x = fmap (const x) o
  o >> n = o >>= const n
    

infixr 6 ======
-- | Infix synonym of 'addHeading'. Intended to be used
--   in @do@ blocks, for headings of presentation slides.
(======) :: Sessionable r => Html -> IPresentation m r -> IPresentation m r
(======) = addHeading

addHeading :: Sessionable r => Html -> IPresentation m r -> IPresentation m r
addHeading h = fmap runIdentity . Encaps (WithHeading h) . Identity

divClass :: Sessionable r => Text -> IPresentation m r -> IPresentation m r
divClass cn = fmap (fst . head . runWriterT)
              . Encaps ManualCSSClasses . WriterT . pure . (,HTMDiv cn)

spanClass :: Sessionable r => Text -> IPresentation m r -> IPresentation m r
spanClass cn = fmap (fst . head . runWriterT)
              . Encaps ManualCSSClasses . WriterT . pure . (,HTMSpan cn)

infix 8 #%
-- | Assign this content a CSS class attribute. If the content is inline, this will
--   be a @<span>@, else a @<div>@.
(#%) :: Sessionable r => Text -> IPresentation m r -> IPresentation m r
c#%q
 | isInline q  = spanClass c q
 | otherwise   = divClass c q

styling :: Css -> IPresentation m r -> IPresentation m r
styling s (Styling s' a) = Styling (s:s') a
styling s a = Styling [s] a

staticContent :: Monoid r => Html -> IPresentation m r
staticContent = fmap (const mempty) . StaticContent

infixr 6 $<>
($<>) :: (r ~ (), TMM.SymbolClass σ, TMM.SCConstraint σ LaTeX)
         => TMM.CAS (TMM.Infix LaTeX) (TMM.Encapsulation LaTeX) (TMM.SymbolD σ LaTeX)
           -> IPresentation m r -> IPresentation m r
($<>) = (<>) . renderTeXMaths MathML.DisplayInline . TMM.toMathLaTeX

maths :: (r ~ (), TMM.SymbolClass σ, TMM.SCConstraint σ LaTeX)
        => [[TMM.CAS (TMM.Infix LaTeX) (TMM.Encapsulation LaTeX) (TMM.SymbolD σ LaTeX)]]
          -> String -> IPresentation m r
maths eqns = renderTeXMaths MathML.DisplayBlock . TMM.maths eqns

renderTeXMaths :: MathML.DisplayType -> LaTeX -> IPresentation m ()
renderTeXMaths dispSty tex = case MathML.readTeX . Txt.unpack $ LaTeX.render tex of
         Right exps -> StaticContent . HTM.preEscapedText . Txt.pack . XML.showElement
                        $ MathML.writeMathML dispSty exps
         Left err -> error $ "Failed to re-parse generated LaTeX. "++err


imageFromFile :: FilePath -> IPresentation IO ()
imageFromFile file = do
   let prepareServing linkPath = do
         isOccupied <- doesPathExist linkPath
         if isOccupied
            then prepareServing $ dropExtension linkPath <> "~1" <.> takeExtension linkPath
            else do
              absOrig <- makeAbsolute file
              createFileLink absOrig linkPath
              let linkFname = takeFileName linkPath
              return $ "pseudostatic"</>linkFname
   imgCode <- serverSide . prepareServing $ pStatDir</>takeFileName file
   StaticContent $ [hamlet| <img src=#{imgCode}> |]()
       

postChPosR :: Handler ()
postChPosR = do
    PositionChange path isRevert <- requireJsonBody
    if isRevert
     then mempty <$> revertProgress path
     else do
        let go, go' :: (PrPath, Text->PrPath, Text) -> [String] -> IPresentation IO r -> Handler (Maybe r)
            go _ [] (StaticContent _) = return $ Just ()
            go _ [] (Pure x) = return $ Just x
            go _ [] (Interactive _ q) = Just <$> liftIO q
            go crumbs path (Encaps (WithHeading _) (Identity cont))
                = fmap Identity <$> go crumbs path cont
            go (crumbh,choiceName,crumbp) [] (Encaps ManualCSSClasses (WriterT conts))
                = sequence . WriterT <$> traverse
                     (\(c,ζ) -> (,ζ) <$> go (crumbh<>case ζ of
                                               HTMDiv i -> " div."<>i
                                               HTMSpan i -> " span."<>i
                                              , choiceName, crumbp) [] c)
                     conts
            go crumbs path (Deterministic f c) = fmap f <$> go crumbs path c
            go _ [] (Resultless c) = return $ Just ()
            go crumbs path (Resultless c) = const(Just()) <$> go crumbs path c
            go crumbs path (Interactive p _) = const Nothing <$> go crumbs path p
            go (crumbh, choiceName, crumbp) (('0':prog):path') (Dependent def _)
                = const Nothing <$> go' (crumbh, choiceName, crumbp<>"0") (prog:path') def
            go (crumbh, choiceName, crumbp) (('1':prog):path') (Dependent def opt) = do
               key <- lookupProgress $ crumbh <> " span."<>choiceName crumbp
               case key of
                 Just k -> go' (crumbh, choiceName, crumbp<>"1") (prog:path') $ opt k
                 Nothing -> do
                   Just k <- go' (crumbh, choiceName, crumbp<>"0") [] def
                   go' (crumbh, choiceName, crumbp<>"1") (prog:path') $ opt k
            go (crumbh,choiceName,crumbp) [[]] (Dependent def opt) = do
               key <- go' (crumbh,choiceName,crumbp<>"0") [[]] def
               case key of
                 Just k -> do
                   setProgress path Nothing k
                   skipContentless (crumbh, choiceName, crumbp<>"1") $ opt k
                   return Nothing
                 Nothing -> error $ outerConstructorName def ++ " refuses to yield a result value."
            go (crumbh, choiceName, crumbp) [] (Dependent _ opt) = do
               key <- lookupProgress $ crumbh <> " span."<>choiceName crumbp
               case key of
                 Just k -> go' (crumbh, choiceName, crumbp<>"1") [] $ opt k
                 Nothing -> return Nothing
            go _ (dir:_) (Dependent _ _)
               = error $ "Div-ID "++dir++" not suitable for making a Dependent choice."
            go crumbs path (Styling _ cont) = go crumbs path cont
            go (crumbh, choiceName, _) (divid:path) (Encaps ManualCSSClasses (WriterT conts))
              | Just dividt <-  HTMDiv<$>Txt.stripPrefix "div." (Txt.pack divid)
                            <|> HTMSpan<$>Txt.stripPrefix "span." (Txt.pack divid)
              , Just subSel <- lookup dividt $ swap<$>conts
                   = fmap (WriterT . pure . (,dividt)) <$> go (crumbh, choiceName, "") path subSel
            go _ [] pres
               = error $ "Need further path information to extract value from a "++outerConstructorName pres
            go _ (dir:_) pres
               = error $ "Cannot index ("++dir++") further into a "++outerConstructorName pres
            go' crumbs path p@(Dependent _ _)  = go crumbs path p
            go' (crumbh,choiceName,crumbp) ([]:t) p
                   = go ( crumbh<>" span."<>choiceName crumbp
                        , disambiguateChoiceName choiceName
                        , "" ) t p
            go' (crumbh,choiceName,crumbp) [] p
             | not $ Txt.null crumbp
                   = go ( crumbh<>" span."<>choiceName crumbp
                        , disambiguateChoiceName choiceName
                        , "" ) [] p
            go' crumbs path p = go crumbs path p
            skipContentless :: (PrPath, Text->PrPath, Text)
                                   -> IPresentation IO r -> Handler (Maybe r)
            skipContentless _ (Pure x) = return $ Just x
            skipContentless crumbs (Interactive p a) = do
               ll <- skipContentless crumbs p
               case ll of
                 Just _ -> Just <$> liftIO a
                 Nothing -> return Nothing
            skipContentless (crumbh,choiceName,crumbp) (Dependent def opt) = do
               let thisDecision = crumbh <> " span."<>choiceName crumbp
               key <- lookupProgress thisDecision
               case key of
                 Just k -> skipContentless (crumbh, choiceName, crumbp<>"1") $ opt k
                 Nothing -> do
                    key' <- skipContentless (crumbh, choiceName, crumbp<>"0") def
                    case key' of
                      Just k' -> do
                        setProgress thisDecision (Just path) k'
                        skipContentless (crumbh, choiceName, crumbp<>"1") $ opt k'
                      Nothing -> return Nothing
            skipContentless crumbs (Styling _ c) = skipContentless crumbs c
            skipContentless crumbs (Resultless c)
                = fmap (const ()) <$> skipContentless crumbs c
            skipContentless crumbs (Deterministic f c)
                = fmap f <$> skipContentless crumbs c
            skipContentless _ (StaticContent _) = return Nothing
            skipContentless _ (Encaps _ _) = return Nothing
            skipContentless _ p = error
             $ "`skipContentless` does not support "++outerConstructorName p
        PresentationServer presentation _ _ <- getYesod
        go ("", defaultChoiceName, "") (finePath <$> Txt.words path) presentation
        return ()
 where finePath p
        | Just prog <- fmap (Txt.dropWhile (=='n'))
                    $ Txt.stripPrefix "span."
                     =<< Txt.stripSuffix "slide" p
           = Txt.unpack prog
        | otherwise  = Txt.unpack p

defaultChoiceName :: Text->PrPath
defaultChoiceName pdiv = "n"<>pdiv<>"slide"

disambiguateChoiceName :: (Text->PrPath) -> (Text->PrPath)
disambiguateChoiceName = (.("n"<>))

getStepBackR :: Handler Html
getStepBackR = do
    undoStack <- lookupSessionJSON "undo-stack"
    mapM undo undoStack
    redirect HomeR
 where undo [] = return ()
       undo (step:steps) = do
          takenStep <- revertProgress step
          case takenStep of
             True -> setSessionJSON "undo-stack" steps
             False -> undo steps

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
   let stepPID = "progress"<>path
   setSessionJSON stepPID prog
   undoStack <- (JSON.decode . BSL.fromStrict =<<) <$> lookupSessionBS "undo-stack"
   setSessionJSON "undo-stack" $ case undoStack of
     Nothing -> [path]
     Just oldSteps -> path : filter (/=path) oldSteps
   forM_ skippedFrom $ setSession ("progress-skip-origin"<>path)

revertProgress :: MonadHandler m => PrPath -> m Bool
revertProgress path = do
   let stepPID = "progress"<>path
   wasPresent <- isJust <$> lookupSessionBS stepPID
   deleteSession stepPID
   let skipOrigKey = "progress-skip-origin"<>path
   lookupSession skipOrigKey >>= mapM_ `id` \skippedFrom -> do
        revertProgress skippedFrom
   deleteSession skipOrigKey
   return wasPresent

lookupSessionJSON :: (MonadHandler m, JSON.FromJSON a) => Text -> m (Maybe a)
lookupSessionJSON = fmap (JSON.decode . BSL.fromStrict =<<) . lookupSessionBS

setSessionJSON :: (MonadHandler m, JSON.ToJSON a) => Text -> a -> m ()
setSessionJSON k = setSessionBS k . BSL.toStrict . JSON.encode

yeamer :: Presentation -> IO ()
yeamer presentation = do
   createDirectoryIfMissing True pStatDir
   pStat <- static pStatDir
   warp 14910 $ PresentationServer (preprocPres presentation) myStatic pStat
