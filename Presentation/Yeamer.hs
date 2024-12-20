-- |
-- Module      : Presentation.Yeamer
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}

module Presentation.Yeamer ( Presentation
                           -- * Running a presentation
                           , yeamer
                           -- * Primitives
                           , staticContent, tweakContent, serverSide
                           -- ** Maths
                           , ($<>), maths
                           -- ** Media content
                           , imageFromFile, mediaFromFile, imageFromFileSupplier
                           -- ** Arbitrary file serving
                           , useFile, useFileSupplier
                           -- ** Code / plaintext
                           , verbatim, plaintext, verbatimWithin
                           -- ** Haskell values
                           , InteractiveShow(..)
                           -- ** Interactive parameters
                           , inputBox, dropdownSelect, triggerButton, feedback_
                           -- * Structure / composition
                           -- ** Headings
                           , addHeading, (======)
                           -- ** In-line composition
                           , module Data.Monoid
                           -- ** Horizontal and vertical composition
                           , module Data.Semigroup.Numbered
                           , table
                           -- ** Interactive value selection
                           , discardResult
                           , (→<>), (→│), (↘──)
                           , (<>←), (│←), (──↖)
                           , (→│←), (↘──↖)
                           , (→<>→), (→│→), (↘──↘)
                           -- * CSS
                           , divClass, divClasses, spanClass, (#%), styling, Css
                           -- * Server configuration
                           , yeamer'
                           , YeamerServerConfig
                           -- | Default port is 14910
                           , yeamerTcpPort
                           -- * Internals
                           , IPresentation
                           ) where

import Yesod hiding (get)
import Yesod.Form.Jquery

import qualified Data.Text as Txt
import qualified Data.Text.Lazy as Txt (toStrict)
import qualified Data.Text.Encoding as Txt
import Data.Text (Text)
import Data.String (IsString (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BC8
import Flat (Flat, flat, unflat)
import qualified Data.Aeson as JSON
import qualified Text.Blaze.Html5 as HTM
import qualified Text.Blaze.Html5.Attributes as HTM
import qualified Text.Blaze.Html.Renderer.Text as HTMText
import Presentation.Yeamer.Internal.Progress
import Presentation.Yeamer.Internal.PrPathStepCompression
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as Arr
import Presentation.Yeamer.Internal.Grid

import Text.Cassius (cassius, Css)
import Text.Julius (rawJS, Javascript, renderJavascript)

import Yesod.Static (Static, static, base64md5)
import Yesod.EmbeddedStatic
import qualified Language.Javascript.JQuery as JQuery
import Language.Haskell.TH.Syntax ( Exp(LitE, AppE, VarE, ConE)
                                  , Lit(StringL), Name, runIO )
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Stack (HasCallStack)
import Data.Int (Int64, Int32, Int16)
import Language.Haskell.TH.Quote

import qualified CAS.Dumb.Symbols as TMM
import qualified CAS.Dumb.Tree as TMM
import qualified Math.LaTeX.Prelude as TMM
import Text.LaTeX (LaTeX)
import qualified Text.LaTeX as LaTeX
import qualified Text.TeXMath as MathML
import qualified Text.XML.Light as XML
import Text.Printf

import Data.List (intercalate)
import Data.Foldable (fold)
import Data.Traversable.Redundancy (rmRedundancy)
import Control.Monad.Trans.Writer.JSONable
import Control.Monad.Trans.List
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Catch (MonadThrow(..), Exception(..))
import Data.These
import Data.These.Lens
import Data.Either (partitionEithers)
import qualified Data.Semigroup as SG
import Data.Semigroup.Numbered
import Data.Monoid
import Data.Maybe
import Data.Functor.Identity
import Control.Monad
import Control.Arrow (first, second, (&&&))
import Control.Applicative

import Data.Typeable(Typeable)

import Data.Function ((&))
import Data.Tuple (swap)
import Data.Proxy
import Data.Default.Class

import System.FilePath ( takeFileName, takeExtension, takeBaseName, dropExtension
                       , (<.>), (</>) )
import System.IO.Error (catchIOError, isAlreadyExistsError)
import System.Directory ( doesPathExist, canonicalizePath, removeFile
                        , createDirectoryIfMissing, renameFile
#if MIN_VERSION_directory(1,3,1)
                        , createFileLink, pathIsSymbolicLink, getSymbolicLinkTarget
#endif
                        )
#if !MIN_VERSION_directory(1,3,1)
import System.Posix.Files (createSymbolicLink, readSymbolicLink)
#endif
import System.IO.Temp

import GHC.Generics
import Lens.Micro
import Lens.Micro.TH
import Data.Bifunctor (bimap)

#if !MIN_VERSION_directory(1,3,1)
createFileLink = createSymbolicLink
getSymbolicLinkTarget = readSymbolicLink
pathIsSymbolicLink _ = pure True
#endif

#if !MIN_VERSION_flat(0,4,0)
instance (Flat a) => Flat (Identity a)
#endif

type Sessionable = Flat

data SessionableWitness a where
  SessionableWitness :: Sessionable a => SessionableWitness a
data EncapsulableWitness t where
  EncapsulableWitness :: (∀ r . Sessionable r => SessionableWitness (t r))
                           -> EncapsulableWitness t

data Container t where
  WithHeading :: Html -> Container Identity
  ManualCSSClasses :: Container (WriterT HTMChunkK [])
  GriddedBlocks :: Container Gridded
  CustomEncapsulation :: EncapsulableWitness t -> (t Html -> Html) -> Container t

simpleConcat :: Container []
simpleConcat = CustomEncapsulation (EncapsulableWitness SessionableWitness) mconcat

data HTMChunkK = HTMDiv {_hchunkCSSClass::Text} | HTMSpan {_hchunkCSSClass::Text}
          deriving (Generic, Eq, Ord, Show)
instance JSON.FromJSON HTMChunkK
instance JSON.ToJSON HTMChunkK
instance Flat HTMChunkK
makeLenses ''HTMChunkK

data IPresentation m r where
   StaticContent :: Html -> IPresentation m ()
   DynamicContent :: m Html -> IPresentation m ()
   TweakableInput :: (Sessionable x, JSON.FromJSON x)
        => Maybe x  -- Default value as the cell's output
         -> (x -> m ()) -- Action to run after each update
         -> ( PrPath -> ( Text   -- The “final leaf” of the DOM path
                        , Maybe x -> -- An already stored value
                           ( PresProgress -> JavascriptUrl (Route PresentationServer)
                           , Html )
                        ))
         -> IPresentation m (Maybe x)
   Resultless :: IPresentation m r -> IPresentation m ()
   Styling :: [Css] -> IPresentation m r -> IPresentation m r
   Encaps :: (Traversable t, Sessionable r, Sessionable rf, Sessionable (t ()))
               => Container t -> (t r -> rf)
                   -> t (IPresentation m r) -> IPresentation m rf
   Pure :: r -> IPresentation m r
   Deterministic :: (r -> Maybe s) -> IPresentation m r -> IPresentation m s
   Interactive :: Sessionable r
          => IPresentation m () -> m r -> IPresentation m r
   Feedback :: Sessionable r
           => (Maybe r -> IPresentation m r) -> IPresentation m r
   Dependent :: Sessionable x
                   => IPresentation m x -> (x -> IPresentation m r)
                         -> IPresentation m (Either x r)
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
/p/#PresProgress ExactPositionR GET
/changeposition/#PresProgress/#PositionChange ChPosR GET
/setvalue/#PresProgress/#PrPath/#ValueToSet SetValR GET
/reset ResetR GET
/static StaticR EmbeddedStatic getStatic
/pseudostatic PStaticR Static getPseudostatic
|]
instance Yesod PresentationServer where
  addStaticContent = embedStaticContent getStatic StaticR Right
  approot = ApprootRelative
instance YesodJquery PresentationServer

preprocPres :: IPresentation m r -> IPresentation m r
preprocPres (StaticContent c) = StaticContent c
preprocPres (DynamicContent c) = DynamicContent c
preprocPres (TweakableInput defV postAct frm) = TweakableInput defV postAct frm
preprocPres (Resultless p) = Resultless $ preprocPres p
preprocPres (Styling s p) = Styling s $ preprocPres p
preprocPres (Encaps (WithHeading h) ff p) = Encaps (WithHeading h) ff $ preprocPres<$>p
preprocPres (Encaps ManualCSSClasses ff p) = Encaps ManualCSSClasses ff $ preprocPres<$>p
preprocPres (Encaps (CustomEncapsulation (EncapsulableWitness w) f) ff p)
                = Encaps (CustomEncapsulation (EncapsulableWitness w) f) ff $ preprocPres<$>p
preprocPres (Encaps GriddedBlocks ff p)
           = Styling grids
           . fmap ff
           . divClass gridClass
           . Encaps ManualCSSClasses (fst . backonstruct . map (first (read . Txt.unpack . _hchunkCSSClass) . swap)
                      . runWriterT)
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
        | c <- toEnum $ fromEnum '㐀' + i
        , c <= '鿋'                       = Txt.singleton c
        | otherwise                       = error "Too many grid cells to display."
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
preprocPres (Feedback f) = Feedback $ preprocPres . f
preprocPres (Dependent d o) = Dependent (preprocPres d) (preprocPres<$>o)


isInline :: IPresentation m a -> Bool
isInline (StaticContent _) = True
isInline (DynamicContent _) = True
isInline (TweakableInput _ _ _) = False
isInline (Encaps ManualCSSClasses _ (WriterT qs)) = all (\(_,i) -> case i of
                   HTMSpan _ -> True
                   HTMDiv _ -> False ) qs
isInline (Encaps _ _ _) = False
isInline (Styling _ q) = isInline q
isInline (Interactive q _) = isInline q
isInline (Feedback f) = False
isInline (Resultless q) = isInline q
isInline (Dependent q _) = isInline q
isInline (Deterministic _ q) = isInline q
isInline (Pure _) = True


getHomeR :: Handler Html
getHomeR = redirect . ExactPositionR $ assemblePresProgress mempty
   

getExactPositionR :: PresProgress -> Handler Html
getExactPositionR pPosition = do
   PresentationServer presentation _ _ <- getYesod
   defaultLayout $ do
      addScript $ StaticR jquery_js
      
      -- Define some CSS variables that can be used as a workaround for the inability of
      -- CSS `calc` to give ratios of sizes back as dimensionless quantities.
      toWidget [julius|
          document.documentElement.style.setProperty('--vw-pixels', window.innerWidth)
          document.documentElement.style.setProperty('--vh-pixels', window.innerHeight)
        |]

      slideChoice <- (`runReaderT`pPosition)
            $ chooseSlide "" defaultChoiceName "" Nothing Nothing presentation
      (`here`slideChoice) $ \slide -> do
          contents <- liftIO $ go 0 slide
          toWidget contents
      return ()
 where chooseSlide :: PrPath -> (Text->PrPath) -> Text -> Maybe PrPath -> Maybe PrPath
                       -> IPresentation IO r
                       -> ReaderT PresProgress
                            (WidgetT PresentationServer IO) (These Presentation r)
       chooseSlide _ _ "" Nothing Nothing (StaticContent conts)
           = pure $ These (StaticContent conts) ()
       chooseSlide _ _ "" Nothing Nothing (DynamicContent conts)
           = pure $ These (DynamicContent conts) ()
       chooseSlide path choiceName pdiv Nothing Nothing (TweakableInput defV _ frm) = do
           let (leafNm, interactor) = frm path
               fullPath = path<>leafNm
           storedValue <- lookupProgress fullPath
           let (action, contents) = interactor storedValue
           toWidget . action =<< ask
           pure $ case (defV, storedValue) of
             (Nothing, Nothing) -> This $ StaticContent contents
             (_, Just r)  -> These (StaticContent contents) (Just r)
             (Just r, _)  -> These (StaticContent contents) (Just r)
       chooseSlide path choiceName "" Nothing Nothing (Styling sty conts)
                     = mapM_ toWidget sty
                        >> chooseSlide path choiceName "" Nothing Nothing conts
       chooseSlide path choiceName "" Nothing Nothing (Encaps f ff conts)
                  = postGather <$> cellwise f
        where postGather sq = case sequence sq of
               That p's -> That $ ff p's
               This _   -> This $ Encaps f (const())
                                $ fmap (maybe mempty id . (^?here)) sq
               These _ p's -> (`These`ff p's) $ Encaps f (const())
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
       chooseSlide path choiceName pdiv bwd fwd (Feedback conts) = do
           prefetch <- chooseSlide path choiceName pdiv bwd fwd $ conts Nothing
           case prefetch ^? there of
             Just v -> chooseSlide path choiceName pdiv bwd fwd . conts $ Just v
             Nothing   -> pure prefetch
       chooseSlide path choiceName "" Nothing Nothing (Resultless conts) = do
           purity <- chooseSlide path choiceName "" Nothing Nothing conts
           case purity ^? here of
             Just pres -> pure . (`These`()) $ discardResult pres
             Nothing   -> pure $ That ()
       chooseSlide path choiceName "" Nothing Nothing (Deterministic f conts)
         = chooseSlide path choiceName "" Nothing Nothing conts
            <&> \case
              This pres -> This pres
              That res -> case f res of
                Just q -> That q
                -- The `Nothing` case should not be possible here, because 
                -- `Deterministic` cannot be directly accessed by the user, and can
                -- only yield `Nothing` is part of `oRDependent`, which guarantees
                -- that there is content. (Not entirely sure about all this, TODO
                -- making it rigorous.)
              These pres res -> case f res of
                Just q -> These pres q
                Nothing -> This pres
       chooseSlide path choiceName pdiv bwd fwd (Dependent def opt) = do
          let progPath = path<>" span."<>choiceName pdiv
          positionCh <- lookupProgress progPath
          case positionCh of
            Nothing -> do
              -- liftIO . putStrLn $ "Not enter '"++Txt.unpack progPath++"'"
              purity <- chooseSlide path choiceName (pdiv<>"0") bwd (Just progPath) def
              case purity of
                 This pres -> pure . This $ discardResult pres
                 That x -> do
                   pPosition' <- setProgress progPath x `execStateT` pPosition
                   redirect $ ExactPositionR pPosition'
                 These pres x -> pure . These (discardResult pres) $ Left x
            Just x -> fmap (fmap Right)
                    . chooseSlide path choiceName (pdiv<>"1") (Just progPath) fwd
                    $ opt x
       chooseSlide path choiceName pdiv bwd fwd pres
        | isJust bwd || isJust fwd  = do
          let thisChoice = choiceName pdiv
              newPath = (path<>" span."<>thisChoice)
              [revertPossible, progressPossible]
                 = maybe "false" (const "true") <$> [bwd,fwd] :: [Text]
              [previous,next] = maybe "null" id <$> [bwd, fwd]
          toWidget [julius|
             $("#{rawJS newPath}").click(function(e){
                 if (e.ctrlKey && #{rawJS revertPossible}) {
                     isRevert = true;
                     pChanger =
                      "@{ChPosR pPosition (PositionChange previous PositionRevert)}";
                 } else if (!(e.ctrlKey) && #{rawJS progressPossible}) {
                     isRevert = false;
                     pChanger =
                      "@{ChPosR pPosition (PositionChange next PositionAdvance)}";
                 } else {
                     return;
                 }
                 e.stopPropagation();
                 hasErrored = false;
                 $.ajax({
                       contentType: "application/json",
                       processData: false,
                       url: pChanger,
                       type: "GET",
                       dataType: "text",
                       success: function(newURL, textStatus, jqXHR) {
                          if (isRevert) {
                             window.location.replace(newURL);
                          } else {
                             window.location.href = newURL;
                          }
                       },
                       error: function(jqXHR, textStatus, errorThrown) {
                          $("body").css("cursor","not-allowed");
                          hasErrored = true;
                          setTimeout(function() {
                             $("body").css("cursor","auto")}, 500);
                       }
                    });
                 setTimeout(function() {
                     if (!hasErrored) {$("body").css("cursor","wait")}
                 }, 150);
             })
            |]
          (here %~ spanClass thisChoice)
                  <$> chooseSlide newPath (disambiguateChoiceName choiceName)
                           "" Nothing Nothing pres
       chooseSlide _ _ _ _ _ (Pure x) = pure $ That x
       chooseSlide _ _ _ _ _ pres
          = error $ "Cannot display "++outerConstructorName pres
       go :: Monad m => Int -> IPresentation m r -> m Html
       go _ (StaticContent conts) = pure conts
       go _ (DynamicContent conts) = conts
       go _ (Pure _) = error $ "Error: impossible to render a slide of an empty presentation."
       go _ (Dependent _ _) = error $ "Internal error: un-selected Dependent option while rendering to HTML."
       go lvl (Deterministic _ conts) = go lvl conts
       go lvl (Resultless conts) = go lvl conts
       go lvl (Interactive conts _) = go lvl conts
       go lvl (Styling sty conts) = go lvl conts
       go lvl (Encaps (WithHeading h) ff conts)
           = let lvl' = min 6 $ lvl + 1
                 hh = [HTM.h1, HTM.h2, HTM.h3, HTM.h4, HTM.h5, HTM.h6]!!lvl
             in go lvl' $ Encaps (CustomEncapsulation (EncapsulableWitness SessionableWitness)
                                         $ \(Identity contsr)
                                    -> HTM.div HTM.! HTM.class_ "headed-container"
                                         $ hh h <> contsr
                                 ) ff conts
       go lvl (Encaps ManualCSSClasses ff conts)
           = go lvl $ Encaps (CustomEncapsulation (EncapsulableWitness SessionableWitness) $ \(WriterT contsrs)
                  -> foldMap (\(q,i) -> case i of
                               HTMDiv c -> [hamlet| <div class=#{withSupclass c}> #{q} |]()
                               HTMSpan c -> [hamlet| <span class=#{withSupclass c}> #{q} |]())
                      $ contsrs
                 ) ff conts
        where withSupclass c
               | Just _ <- Txt.stripPrefix "autogrid_" c
                            = "autogrid "<>c
               | otherwise  = c
       go lvl (Encaps (CustomEncapsulation (EncapsulableWitness _) f) _ conts)
             = fmap f . forM conts $ go lvl
       go _ p = error $ outerConstructorName p <> " cannot be rendered."


hchunkFor :: Text -> IPresentation m r -> HTMChunkK
hchunkFor t p | isInline p  = HTMSpan t
              | otherwise   = HTMDiv t

instance (Monad m, Monoid r, Sessionable r) => SG.Semigroup (IPresentation m r) where
  StaticContent c <> StaticContent d = StaticContent $ c<>d
  DynamicContent c <> DynamicContent d = DynamicContent $ liftA2 (<>) c d
  StaticContent c <> DynamicContent d = DynamicContent $ (c<>)<$>d
  DynamicContent c <> StaticContent d = DynamicContent $ (<>d)<$>c
  Encaps ManualCSSClasses ff₀ (WriterT elems₀) <> Encaps ManualCSSClasses ff₁ (WriterT elems₁)
     = Encaps ManualCSSClasses ff' . WriterT . disambiguate
           $ map (first $ fmap Left) elems₀ ++ map (first $ fmap Right) elems₁
   where disambiguate = go 0 Map.empty
          where go _ _ [] = []
                go i occupied ((q,c):qs)
                 | Txt.null (_hchunkCSSClass c) || c`Map.member`occupied
                    = let c' = c & hchunkCSSClass .~ Txt.pack ("anonymousCell-"++show i)
                      in go (i+1) occupied (( Encaps ManualCSSClasses (fst . head . runWriterT) $ WriterT [(q,c)]
                                            , c' ):qs)
                 | otherwise
                    = (q,c) : go (i+1) (Map.insert c () occupied) qs
         ff' (WriterT l) = case partitionEithers
                             $ map ( \(d,r)->case d of Left d'->Left (d',r)
                                                       Right d'->Right (d',r) ) l of
                  (l₀,l₁) -> ff₀ (WriterT l₀)<>ff₁ (WriterT l₁)
  Resultless (Encaps ManualCSSClasses _ ps) <> Resultless (Encaps ManualCSSClasses _ qs)
      = Resultless $ Encaps ManualCSSClasses id (discardResult<$>ps)
               SG.<> Encaps ManualCSSClasses id (discardResult<$>qs)
  Resultless p@(Encaps ManualCSSClasses _ _) <> c
      = Resultless p <> Resultless (Encaps ManualCSSClasses id $ WriterT [(c,hchunkFor""c)])
  c <> Resultless p@(Encaps ManualCSSClasses _ _)
      = Resultless (Encaps ManualCSSClasses id $ WriterT [(c,hchunkFor""c)]) <> Resultless p
  p <> q = Encaps ManualCSSClasses fold $ WriterT
             [(p, hchunkFor"anonymousCell-0"p), (q, hchunkFor"anonymousCell-1"q)]
instance ∀ m . Monad m => Monoid (IPresentation m ()) where
  mappend = (SG.<>)
  mempty = Resultless $ Encaps ManualCSSClasses id
                (WriterT [] :: WriterT HTMChunkK [] (IPresentation m ()))

infixr 6 →<>
(→<>) :: (Sessionable a)
    => IPresentation m a -> IPresentation m b -> IPresentation m a
l→<>r = fmap (\[Just a, Nothing] -> a)
       . Encaps simpleConcat id
       $ [ Just<$>l, const Nothing<$>r ]

infix 6 →│
(→│) :: (Sessionable a)
    => IPresentation m a -> IPresentation m b -> IPresentation m a
l→│r = fmap (\(GridDivisions [[GridRegion (Just a), GridRegion Nothing]]) -> a)
       . Encaps GriddedBlocks id
       $ GridDivisions [GridRegion<$>[ Just<$>l, const Nothing<$>r ]]

infix 5 ↘──
(↘──) :: (Sessionable a)
    => IPresentation m a -> IPresentation m b -> IPresentation m a
l↘──r = fmap (\(GridDivisions [[GridRegion (Just a)], [GridRegion Nothing]]) -> a)
       . Encaps GriddedBlocks id
       $ GridDivisions [[GridRegion $ Just<$>l], [GridRegion $ const Nothing<$>r]]

infixr 6 <>←
(<>←) :: (Sessionable b)
    => IPresentation m a -> IPresentation m b -> IPresentation m b
l<>←r = fmap (\[Nothing, (Just b)] -> b)
       . Encaps simpleConcat id
       $ [ const Nothing<$>l, Just<$>r ]

infix 6 │←
(│←) :: (Sessionable b)
    => IPresentation m a -> IPresentation m b -> IPresentation m b
l│←r = fmap (\(GridDivisions [[GridRegion Nothing, GridRegion (Just b)]]) -> b)
       . Encaps GriddedBlocks id
       $ GridDivisions [GridRegion<$>[ const Nothing<$>l, Just<$>r ]]

infix 5 ──↖
(──↖) :: (Sessionable b)
    => IPresentation m a -> IPresentation m b -> IPresentation m b
l──↖r = fmap (\(GridDivisions [[GridRegion Nothing, GridRegion (Just b)]]) -> b)
       . Encaps GriddedBlocks id
       $ GridDivisions [[GridRegion $ const Nothing<$>l], [GridRegion $ Just<$>r]]

infix 6 →<>→
(→<>→) :: (Sessionable a, Monad m)
    => IPresentation m a -> (a -> IPresentation m ()) -> IPresentation m a
l→<>→r = Feedback $ \aFbq -> l →<> case aFbq of
                                  Just a -> r a
                                  Nothing -> mempty

infix 6 →│→
(→│→) :: (Sessionable a, Monad m)
    => IPresentation m a -> (a -> IPresentation m ()) -> IPresentation m a
l→│→r = Feedback $ \aFbq -> l →│ case aFbq of
                                  Just a -> r a
                                  Nothing -> mempty

infix 5 ↘──↘
(↘──↘) :: (Sessionable a, Monad m)
    => IPresentation m a -> (a -> IPresentation m ()) -> IPresentation m a
l↘──↘r = Feedback $ \aFbq -> l ↘── case aFbq of
                                  Just a -> r a
                                  Nothing -> mempty

-- -- This should in principle work, but doesn't because inline subexpressions share
-- -- the same path in the Yeamer model and would thus not get separate ids we could
-- -- handle for each of the values:
-- infix 6 →<>←
-- (→<>←) :: (Sessionable a, Sessionable b)
--     => IPresentation m a -> IPresentation m b -> IPresentation m (a,b)
-- l→<>←r = fmap (\[Left a, Right b] -> (a,b))
--        . Encaps simpleConcat id
--        $ [Left<$>l, Right<$>r]

infix 6 →│←
(→│←) :: (Sessionable a, Sessionable b)
    => IPresentation m a -> IPresentation m b -> IPresentation m (a,b)
l→│←r = fmap (\(GridDivisions [[GridRegion (Left a), GridRegion (Right b)]])
                 -> (a,b))
       . Encaps GriddedBlocks id
       $ GridDivisions [GridRegion<$>[Left<$>l, Right<$>r]]

infix 5 ↘──↖
(↘──↖) :: (Sessionable a, Sessionable b)
    => IPresentation m a -> IPresentation m b -> IPresentation m (a,b)
l↘──↖r = fmap (\(GridDivisions [[GridRegion (Left a)], [GridRegion (Right b)]])
                 -> (a,b))
       . Encaps GriddedBlocks id
       $ GridDivisions [[GridRegion $ Left<$>l], [GridRegion $ Right<$>r]]

instance ∀ m . SemigroupNo 0 (IPresentation m ()) where
  sappendN _ (Resultless (Encaps GriddedBlocks _ l))
             (Resultless (Encaps GriddedBlocks _ r))
           = Resultless . Encaps GriddedBlocks id $ (discardResult<$>l) │ (discardResult<$>r)
  sappendN _ l@(Resultless (Encaps GriddedBlocks _ _)) r
           = l │ Resultless (Encaps GriddedBlocks id $ pure r)
  sappendN _ l r
           = Resultless (Encaps GriddedBlocks id $ pure l) │ r

instance ∀ m . SemigroupNo 1 (IPresentation m ()) where
  sappendN _ (Resultless (Encaps GriddedBlocks _ t))
             (Resultless (Encaps GriddedBlocks _ b))
           = Resultless . Encaps GriddedBlocks id $ (discardResult<$>t) ── (discardResult<$>b)
  sappendN _ t@(Resultless (Encaps GriddedBlocks _ _)) b
           = t ── Resultless (Encaps GriddedBlocks id $ pure b)
  sappendN _ t b
           = Resultless (Encaps GriddedBlocks id $ pure t) ── b


outerConstructorName :: IPresentation m r -> String
outerConstructorName (StaticContent _) = "StaticContent"
outerConstructorName (DynamicContent _) = "DynamicContent"
outerConstructorName (TweakableInput _ _ _) = "TweakableInput"
outerConstructorName (Resultless _) = "Resultless"
outerConstructorName (Styling _ _) = "Styling"
outerConstructorName (Encaps (WithHeading _) _ _) = "Encaps WithHeading"
outerConstructorName (Encaps (CustomEncapsulation _ _) _ _) = "Encaps CustomEncapsulation"
outerConstructorName (Encaps ManualCSSClasses _ _) = "Encaps ManualCSSClasses"
outerConstructorName (Encaps GriddedBlocks _ _) = "Encaps GriddedBlocks"
outerConstructorName (Pure _) = "Pure"
outerConstructorName (Deterministic _ _) = "Deterministic"
outerConstructorName (Interactive _ _) = "Interactive"
outerConstructorName (Dependent _ _) = "Dependent"

discardResult :: IPresentation m r -> IPresentation m ()
discardResult (StaticContent c) = StaticContent c
discardResult (DynamicContent c) = DynamicContent c
discardResult (Resultless p) = Resultless p
discardResult p = Resultless p

feedback_ :: Sessionable a => (Maybe a -> IPresentation m a) -> IPresentation m ()
feedback_ = discardResult . Feedback

-- | Run a monadic action and use the result in the presentation.
--   Note that the action might not be re-run even if it depends to other
--   values chosen at another point in the presentation, so use with care.
serverSide :: Sessionable a => m a -> IPresentation m a
serverSide = Interactive (pure ())

instance Functor (IPresentation m) where
  fmap f (Deterministic g q) = Deterministic (fmap f . g) q
  fmap f (Pure x) = Pure $ f x
  fmap f q = Deterministic (Just . f) q
instance Applicative (IPresentation m) where
  pure = Pure
  Pure f <*> x = fmap f x
  f <*> Pure x = fmap ($ x) f
  fs<*>xs = ap fs xs

onlyRight :: IPresentation m (Either a b) -> IPresentation m b
onlyRight = Deterministic $ \case
    Left _ -> Nothing
    Right y -> Just y

oRDependent :: Flat x
    => IPresentation m x -> (x -> IPresentation m r) -> IPresentation m r
oRDependent a b = onlyRight $ Dependent a b

instance ∀ m . Monad (IPresentation m) where
  return = pure
  StaticContent c >>= f = oRDependent (StaticContent c) f
  DynamicContent c >>= f = oRDependent (DynamicContent c) f
  TweakableInput defV postAct frm >>= f = oRDependent (TweakableInput defV postAct frm) f
  Resultless p >>= f = oRDependent (Resultless p) f
  Feedback p >>= f = oRDependent (Feedback p) f
  Styling _ (Pure x) >>= f = f x
  Styling s (StaticContent c) >>= f = oRDependent (Styling s (StaticContent c)) f
  Styling s (DynamicContent c) >>= f = oRDependent (Styling s (DynamicContent c)) f
  Styling s (Resultless c) >>= f = oRDependent (Styling s (Resultless c)) f
  Styling s (Styling s' x) >>= f = Styling (s++s') x >>= f
  Styling s (Encaps (WithHeading h) ff x) >>= f
      = oRDependent (Styling s (Encaps (WithHeading h) ff x)) f
  Styling s (Encaps ManualCSSClasses ff x) >>= f
      = oRDependent (Styling s (Encaps ManualCSSClasses ff x)) f
  Styling s (Deterministic g x) >>= f = Deterministic g (Styling s x) >>= f
  Styling s (Interactive p o) >>= f = oRDependent (Interactive (Styling s p) o) f
  Styling s (Dependent p g) >>= f
      = oRDependent (Styling s p) $ \x -> Styling s (g x) >>= f . Right
  Encaps (WithHeading h) ff p >>= f = oRDependent (Encaps (WithHeading h) ff p) f
  Encaps ManualCSSClasses ff ps >>= f = oRDependent (Encaps ManualCSSClasses ff ps) f
  Encaps (CustomEncapsulation (EncapsulableWitness w') e') ff' ps' >>= f'
      = bindCustEncaps w' e' ff' ps' f'
   where bindCustEncaps :: ∀ a b t r tr
                   . (Sessionable r, Sessionable tr, Traversable t, Sessionable (t ()))
                   => (∀ r' . Sessionable r' => SessionableWitness (t r'))
                   -> (t Html -> Html)
                   -> (t r -> tr)
                   -> t (IPresentation m r)
                   -> (tr -> IPresentation m b)
                   -> IPresentation m b
         bindCustEncaps w e ff ps f
          = case w :: SessionableWitness (t r) of
             SessionableWitness
               -> oRDependent (Encaps (CustomEncapsulation (EncapsulableWitness w) e) ff ps) f
  Pure x >>= f = f x
  Deterministic g p >>= f = onlyRight $
           p >>= \x -> case g x of
                Just y -> Right <$> f y
                Nothing -> Left <$> p
  Interactive p q >>= f = oRDependent (Interactive p q) f
  Dependent p g >>= f = oRDependent p $ g >=> f . Right
  o >> Interactive (Pure _) q = Interactive (discardResult o) q
  o >> Pure x = fmap (const x) o
  o >> n = o >>= const n
    

infixr 6 ======
-- | Infix synonym of 'addHeading'. Intended to be used
--   in @do@ blocks, for headings of presentation slides.
(======) :: Sessionable r => Html -> IPresentation m r -> IPresentation m r
(======) = addHeading

addHeading :: Sessionable r => Html -> IPresentation m r -> IPresentation m r
addHeading h = Encaps (WithHeading h) runIdentity . Identity

divClass :: Sessionable r => Text -> IPresentation m r -> IPresentation m r
divClass cn = Encaps ManualCSSClasses (fst . head . runWriterT) . WriterT . pure . (,HTMDiv cn)

spanClass :: Sessionable r => Text -> IPresentation m r -> IPresentation m r
spanClass cn = Encaps ManualCSSClasses (fst . head . runWriterT) . WriterT . pure . (,HTMSpan cn)

divClasses :: Sessionable r => [(Text, IPresentation m r)] -> IPresentation m r
divClasses cns = Encaps ManualCSSClasses (fst . head . runWriterT) $ WriterT [ (content,HTMDiv cn)
                                                  | (cn,content) <- cns ]

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

tweakContent :: Sessionable r => (Html -> Html) -> IPresentation m r -> IPresentation m r
tweakContent f = Encaps (CustomEncapsulation (EncapsulableWitness SessionableWitness)
                              $ f . runIdentity) runIdentity
               . Identity

type JSCode url0 = (url0 -> [(Text,Text)] -> Text) -> Javascript

class Sessionable i => Inputtable i where
  inputElemHtml :: i      -- ^ Current value
                -> String -- ^ id in the DOM
                -> Html
  inputElemJSRead :: String   -- ^ id in the DOM
                  -> Javascript -- ^ Expression that reads out the value
  inputElemJSRead inputElId
      = [julius| $("#{rawJS inputElId}").val() |](\_ _ -> mempty)

instance Inputtable Int where
  inputElemHtml currentVal hashedId = [hamlet|
        <input type="number" id="#{hashedId}" value=#{currentVal}>
       |]()

instance Inputtable Double where
  inputElemHtml currentVal hashedId = [hamlet|
        <input type="number" id="#{hashedId}" value=#{currentVal} step="any">
       |]()

instance Inputtable String where
  inputElemHtml currentVal hashedId = [hamlet|
        <input type="string" id="#{hashedId}" value=#{currentVal}>
       |]()
  inputElemJSRead inputElId
      = [julius| JSON.stringify($("#{rawJS inputElId}").val()) |](\_ _ -> mempty)

inputBox :: ∀ i m . (Inputtable i, JSON.FromJSON i, Applicative m)
                => i -> IPresentation m i
inputBox iDef = fmap (maybe iDef id) . TweakableInput (Just iDef) (const $ pure ())
  $ \path ->
      let hashedId = base64md5 . BSL.fromStrict $ Txt.encodeUtf8 path
          inputElId = "input#"++hashedId
      in ( leafNm
         , \prevInp ->
            let currentVal = case prevInp of
                 Nothing -> iDef
                 Just v -> v
                valueReader = rawJS . renderJavascript $ inputElemJSRead @i inputElId
            in ( \pPosition -> [julius|
                 $("#{rawJS inputElId}").click(function(e){
                     e.stopPropagation();
                   })
                 $("#{rawJS inputElId}").change(function(e){
                     currentVal = #{valueReader}
                     pChanger =
                          "@{SetValR pPosition path NoValGiven}".slice(0, -1)
                                // The slice hack removes the `NoValGiven`, to
                                // be replaced with the actual value:
                          + currentVal;
                     e.stopPropagation();
                     hasErrored = false;
                     $.ajax({
                           contentType: "application/json",
                           processData: false,
                           url: pChanger,
                           type: "GET",
                           dataType: "text",
                           success: function(newURL, textStatus, jqXHR) {
                              window.location.href = newURL;
                           },
                           error: function(jqXHR, textStatus, errorThrown) {
                              $("body").css("cursor","not-allowed");
                              hasErrored = true;
                              setTimeout(function() {
                                 $("body").css("cursor","auto")}, 500);
                           }
                        });
                     setTimeout(function() {
                         if (!hasErrored) {$("body").css("cursor","wait")}
                     }, 150);
                 })
                      |]
               , inputElemHtml currentVal hashedId
               ) )
 where leafNm = " input"

dropdownSelect :: ∀ a m . Applicative m
          => (a -> String) -> [a] -> Int -> IPresentation m a
dropdownSelect valShow options iDef
  | iDef>=0 && iDef<length options
      = fmap ((options!!) . maybe iDef id) . TweakableInput (Just iDef) (const $ pure ())
     $ \path ->
       let hashedId = base64md5 . BSL.fromStrict $ Txt.encodeUtf8 path
           selectElId = "select#"++hashedId
       in ( leafNm
          , \prevInp ->
             let currentIndex = case prevInp of
                  Nothing -> iDef
                  Just i -> i
             in ( \pPosition -> [julius|
                  $("#{rawJS selectElId}").click(function(e){
                      e.stopPropagation();
                    })
                  $("#{rawJS selectElId}").change(function(e){
                      currentVal = $("#{rawJS selectElId}").val()
                      pChanger =
                           "@{SetValR pPosition path NoValGiven}".slice(0, -1)
                                 // The slice hack removes the `NoValGiven`, to
                                 // be replaced with the actual value:
                           + currentVal;
                      e.stopPropagation();
                      hasErrored = false;
                      $.ajax({
                            contentType: "application/json",
                            processData: false,
                            url: pChanger,
                            type: "GET",
                            dataType: "text",
                            success: function(newURL, textStatus, jqXHR) {
                               window.location.href = newURL;
                            },
                            error: function(jqXHR, textStatus, errorThrown) {
                               $("body").css("cursor","not-allowed");
                               hasErrored = true;
                               setTimeout(function() {
                                  $("body").css("cursor","auto")}, 500);
                            }
                         });
                      setTimeout(function() {
                          if (!hasErrored) {$("body").css("cursor","wait")}
                      }, 150);
                  })
                       |]
                , let ixedOptions = zip [0..] $ valShow<$>options
                      isCurrentIndex = (==currentIndex)
                  in [hamlet|
                      <select id="#{hashedId}">
                        $forall (i, optionStr) <- ixedOptions
                           $if isCurrentIndex i
                              <option value="#{i}" selected>
                                 #{optionStr}
                           $else
                              <option value="#{i}">
                                 #{optionStr}
                      |]()
                ) )
  | otherwise  = error "Default selection for dropdown not included in options list."
 where leafNm = " select"

triggerButton :: ∀ m . String -> m () -> IPresentation m ()
triggerButton label action = fmap (const ())
    . TweakableInput (Just (0::Int)) (const action)
     $ \path -> 
       let hashedId = base64md5 . BSL.fromStrict $ Txt.encodeUtf8 path
           selectElId = "button#"++hashedId
       in ( leafNm
          , \prevInp ->
                ( \pPosition -> [julius|
                  $("#{rawJS selectElId}").click(function(e){
                      e.stopPropagation();
                      currentVal = 0
                      pChanger =
                           "@{SetValR pPosition path NoValGiven}".slice(0, -1)
                                 // The slice hack removes the `NoValGiven`, to
                                 // be replaced with the actual value:
                           + currentVal;
                      hasErrored = false;
                      $.ajax({
                            contentType: "application/json",
                            processData: false,
                            url: pChanger,
                            type: "GET",
                            dataType: "text",
                            success: function(newURL, textStatus, jqXHR) {
                               window.location.href = newURL;
                            },
                            error: function(jqXHR, textStatus, errorThrown) {
                               $("body").css("cursor","not-allowed");
                               hasErrored = true;
                               setTimeout(function() {
                                  $("body").css("cursor","auto")}, 500);
                            }
                         });
                      setTimeout(function() {
                          if (!hasErrored) {$("body").css("cursor","wait")}
                      }, 150);
                  })
                       |]
                , [hamlet|
                      <button type="button" id="#{hashedId}">
                        #{label}
                      |]()
                ) )
 where leafNm = " button"

infixr 6 $<>

-- | Include a mathematical expression inline in the document.
($<>) :: (r ~ (), TMM.SymbolClass σ, TMM.SCConstraint σ LaTeX, Monad m)
         => TMM.CAS (TMM.Infix LaTeX) (TMM.Encapsulation LaTeX) (TMM.SymbolD σ LaTeX)
           -> IPresentation m r -> IPresentation m r
($<>) = (<>) . renderTeXMaths MathML.DisplayInline . TMM.toMathLaTeX

-- | Include a mathematical expression as a “display” in the document, typically used for equations.
-- 
--   Example:
-- 
--   @
--     "The constant "<>π$<>" fulfills "
--       <>maths [[ sin π⩵0 ]]"."
--   @
-- 
--   The maths expressions use TeX-my-maths syntax ("Math.LaTeX.Prelude").
--   Note that TeX-my-maths has different syntax flavours (e.g. @𝑎×𝑒◝γ@ vs. @a * exp gamma@).
--   Both can be used with '$<>' and 'maths', but the downside is type ambiguity
--   in expressions that include no symbols at all (e.g. single numbers), which can lead to
--   @Could not deduce ‘SymbolClass σ’@ compiler errors. To avoid this problem, you
--   can add local type signatures or use the "Presentation.Yeamer.Maths.Unicode.MathLatin_RomanGreek"
--   module.
maths :: (r ~ (), TMM.SymbolClass σ, TMM.SCConstraint σ LaTeX)
        => [[TMM.CAS (TMM.Infix LaTeX) (TMM.Encapsulation LaTeX) (TMM.SymbolD σ LaTeX)]]
          -> String -> IPresentation m r
maths eqns = renderTeXMaths MathML.DisplayBlock . TMM.maths eqns

renderTeXMaths :: MathML.DisplayType -> LaTeX -> IPresentation m ()
renderTeXMaths dispSty tex = case MathML.readTeX
#if !MIN_VERSION_texmath(0,12,0)
                                   . Txt.unpack
#endif
                                   $ LaTeX.render tex of
         Right exps -> StaticContent . HTM.preEscapedText . Txt.pack . XML.showElement
                        $ MathML.writeMathML dispSty exps
         Left err -> error $ "Failed to re-parse generated LaTeX. "++
#if MIN_VERSION_texmath(0,12,0)
                               Txt.unpack
#endif
                                 err

-- | Include a piece of plaintext, preserving all formatting. To be used in an
--   oxford bracket.
--
--   In practice, you probably want to use this for monospace plaintext, which should
--   appear in a @<pre>@ or @<textarea>@ tag. Use the specialised quoters for that.
verbatim :: QuasiQuoter  -- ^ ≈ @'String' -> 'IPresentation' m ()@
verbatim = verbatimWithin 'id

-- | Convenience wrapper to generate quasi-quoters that will wrap code in any suitable
--   HTML environment.
verbatimWithin :: Name         -- ^ A function @'Html' -> 'Html'@ that should be
                               --   used for presenting the (pre-escaped) plaintext.
               -> QuasiQuoter  -- ^ A specialised version of 'verbatim' that will
                               --   always use the wrapper.
verbatimWithin env
          = QuasiQuoter (pure . verbExp . preproc) undefined undefined undefined
 where verbExp = AppE (ConE 'StaticContent)
             . AppE (VarE env) . AppE (VarE 'HTM.preEscapedString) . LitE . StringL
       preproc s
         | (initL, '\n':posNewl) <- break (=='\n') s
         , all (==' ') initL
                      = preescapeBlock posNewl
         | otherwise  = preescapeInline s
       preescapeBlock s = intercalate "<br>" $ drop zeroIndent <$> sLines
        where zeroIndent = minimum $ length . takeWhile (==' ') <$> sLines
              sLines = preescapeInline <$> lines s
       preescapeInline = concatMap preescapeChar
       preescapeChar '<' = "&lt;"
       preescapeChar '>' = "&gt;"
       preescapeChar '&' = "&amp;"
       preescapeChar c = [c]

-- | A simple version of 'verbatim' that gives the HTML wrapped in @<pre>@ tags, so
--   it will (by default) appear in a monospace font.
plaintext :: QuasiQuoter  -- ^ ≈ @'String' -> 'IPresentation' m ()@
plaintext = verbatimWithin 'HTM.pre
       
-- | Display an image generated on-the-fly in the server. The image will be
--   stored temporarily, in a content-indexed fashion.
imageFromFileSupplier
   :: String               -- ^ File extension
   -> (FilePath -> IO ())  -- ^ File-writer function. This will be called every time
                           --   a slide with the image is requested.
   -> IPresentation IO ()
imageFromFileSupplier ext = includeMediaFile SimpleImage ext . Left

-- | Display an image that lies on the server as any ordinary static file.
--   This is a special case of 'useFile', wrapping the file in an @<img>@ tag.
imageFromFile :: FilePath -> IPresentation IO ()
imageFromFile file = includeMediaFile SimpleImage (takeExtension file) $ Right file

-- | More general form of 'imageFromFile'. Takes a guess based on the file
--   extension, as to whether the media is a standing image or a video. In the
--   latter case, simple HTML5 controls are added.
mediaFromFile :: FilePath -> IPresentation IO ()
mediaFromFile file = includeMediaFile (guessMediaFileSetup fileExt) fileExt $ Right file
 where fileExt = takeExtension file

guessMediaFileSetup :: String -> FileUsageSetup
guessMediaFileSetup fileExt
  | fileExt`elem`knownImgFormats    = SimpleImage
  | fileExt`elem`knownVideoFormats  = SimpleVideo
  | otherwise  = error $ "Unknow media format ‘"++fileExt++"’. Use one of "
                            ++ show (knownImgFormats++knownVideoFormats)
 where knownImgFormats = ('.':)<$>words "png gif jpg jpeg"
       knownVideoFormats = ('.':)<$>words "webm ogg mp4"

data FileUsageSetup
      = SimpleImage
      | SimpleVideo
      | CustomFile (Url -> Html)

type Url = FilePath

useFile :: FilePath            -- ^ File that should be served to the client
        -> (Url -> Html)       -- ^ How it should be used in the presentation
        -> IPresentation IO ()
useFile file use = includeMediaFile (CustomFile use) (takeExtension file) $ Right file

useFileSupplier :: String               -- ^ File extension
                -> (FilePath -> IO ())  -- ^ Server-side file-providing action
                -> (Url -> Html)        -- ^ How to use the file client-side
                -> IPresentation IO ()
useFileSupplier ext supplier use = includeMediaFile (CustomFile use) ext $ Left supplier

includeMediaFile :: FileUsageSetup -> FilePath
             -> Either (FilePath -> IO ()) FilePath -> IPresentation IO ()
includeMediaFile mediaSetup fileExt fileSupp = DynamicContent $ do
   let prepareServing file hashLen completeHash = do
         let linkPath = pStatDir</>take hashLen completeHash<.>takeExtension file
         isOccupied <- doesPathExist linkPath
         let codedName = takeBaseName linkPath
             makeThisLink = do
               createFileLink file linkPath
               return codedName
             disambiguate
              | hashLen < length linkPath  = prepareServing file (hashLen+1) completeHash
         if isOccupied
            then do
              isSymlk <- pathIsSymbolicLink linkPath
              if isSymlk
                then do
                  existingTgt <- getSymbolicLinkTarget linkPath
                  if existingTgt/=file
                   then error $ "Hash collision for path `"<>linkPath<>"` between files `"<>existingTgt<>"` and `"<>file<>"`"
                   else return codedName
                else error $ "A non-link file in occupies the needed path `"<>linkPath<>"`"
            else do
              catchIOError makeThisLink $ \e -> do
                 if isAlreadyExistsError e
                  then do
                   removeFile linkPath -- must have been a dead link anyway, else it
                   makeThisLink        -- would have already triggered `doesPathExist`.
                  else
                   error $ "Unknown problem when creating link `"<>linkPath<>"`"
   imgCode <- case fileSupp of
       Right file -> do
         absOrig <- canonicalizePath file
         prepareServing absOrig 10 . base64md5 . BSL.fromStrict . BC8.pack
                 $ show absOrig
       Left supplier -> do
               tmpFile <- emptyTempFile pStatDir fileExt
               supplier tmpFile
               longHash <- base64md5 <$> BSL.readFile tmpFile
               let file = longHash<.>fileExt
               renameFile tmpFile (pStatDir</>file)
               prepareServing file 10 (base64md5 . BSL.fromStrict $ BC8.pack file)
               
   let servableFile = "/pseudostatic"</>imgCode<.>fileExt
   return $ case mediaSetup of
         SimpleImage -> [hamlet| <img src=#{servableFile}> |]()
         SimpleVideo -> [hamlet| <video src=#{servableFile} controls> |]()
         CustomFile use -> use servableFile


getSetValR :: PresProgress -> PrPath -> ValueToSet -> Handler Text
getSetValR oldPosition path
    = getChPosR oldPosition . PositionChange path . PositionSetValue

getChPosR :: PresProgress -> PositionChange -> Handler Text
getChPosR oldPosition posStep = do
   newPosition <- execStateT (changePos_State posStep) oldPosition
   toTextUrl $ ExactPositionR newPosition

newtype PositionStateJSONError = PositionStateJSONError String
 deriving (Show, Typeable)
instance Exception PositionStateJSONError

changePos_State :: PositionChange -> StateT PresProgress Handler ()
changePos_State (PositionChange path pChangeKind) = do
    PresentationServer presentation _ _ <- getYesod
    go ("", defaultChoiceName, "") (finePath <$> Txt.words path) presentation
    return ()

 where -- | Traverse into the presentation tree structure to find a suitable
       --   key to set at the relevant position.
       --   'go'' will disambiguate path names of different branches.
       go, go' :: ( PrPath            -- The path traversed already
                  , Text->PrPath      -- How to use new path-chunks
                  , Text )            -- Path-chunk being constructed
               -> [String]            -- Path yet to traverse
               -> IPresentation IO r  -- Presentation which to proceed
               -> StateT PresProgress Handler
                           ( Maybe r  -- Key value this branch yields
                           , Bool )   -- Whether it contains displayable content
       go _ [] (StaticContent _) = return $ (Just (), True)
       go _ [] (DynamicContent _) = return $ (Just (), True)
       go (crumbh,choiceName,crumbp) [] (TweakableInput defV postAct twInp) = do
          let (pathFin, _) = twInp crumbh
              fullPath = crumbh<>pathFin
          case pChangeKind of
           PositionSetValue (ValueToSet newVal)
            -> case JSON.fromJSON newVal of
              JSON.Success v -> do
               setProgress fullPath v
               liftIO $ postAct v
               return (Just $ Just v, True)
              JSON.Error e -> throwM $ PositionStateJSONError e
           _ -> do
             key <- lookupProgress fullPath
             let result = case (key, defV) of
                  (Just v, _) -> Just v
                  (_, Just v) -> Just v
                  (Nothing, Nothing) -> Nothing
             return $ (Just result, True)
       go _ [] (Pure x) = return $ (Just x, False)
       go _ [] (Interactive _ q)
           = (,error "Don't know if interactive request actually shows something.")
             . Just <$> liftIO q
       go crumbs path (Encaps (WithHeading _) ff (Identity cont))
           = (,True) . fmap (ff . Identity) . fst <$> go crumbs path cont
       go crumbs path (Encaps (CustomEncapsulation _ _) ff cont)
           = (,True) . fmap ff . sequence
              <$> traverse (\c -> fst <$> go crumbs path c) cont
       go (crumbh,choiceName,crumbp) [] (Encaps ManualCSSClasses ff (WriterT conts))
           = (,True) . fmap ff . sequence . WriterT <$> traverse
                (\(c,ζ) -> (,ζ) . fst <$> go (crumbh<>case ζ of
                                              HTMDiv i -> " div."<>i
                                              HTMSpan i -> " span."<>i
                                             , choiceName, crumbp) [] c)
                conts
       go crumbs path (Deterministic f c) = first (f=<<) <$> go crumbs path c
       go crumbs [] (Resultless c) = case pChangeKind of
         PositionSetValue _ -> first (fmap $ const ()) <$> go crumbs [] c
         -- TODO check if the second branch actually needs special handling
         _ -> (Just (),) <$> hasDisplayableContent crumbs c
       go crumbs path (Resultless c) = first (const $ Just()) <$> go crumbs path c
       go crumbs path (Interactive p _) = first (const Nothing) <$> go crumbs path p
       go crumbs path (Feedback p) = go crumbs path $ p Nothing
       go (crumbh, choiceName, crumbp) (('0':prog):path') (Dependent def _)
           = first (const Nothing)
                <$> go' (crumbh, choiceName, crumbp<>"0") (prog:path') def
       go (crumbh, choiceName, crumbp) path' (Dependent def opt) = do
          key <- lookupProgress $ crumbh <> " span."<>choiceName crumbp
          case (key, path', pChangeKind) of
            (Just k, ('1':prog):path'', _) -> do
              (resKey, rHasContent)
                   <- go' (crumbh, choiceName, crumbp<>"1") (prog:path'') $ opt k
              if pChangeKind==PositionRevert && not rHasContent then do
                 revertProgress $ crumbh <> " span."<>choiceName crumbp
                 return (Nothing, False)
               else return (Right<$>resKey, rHasContent)
            (Nothing, ('1':prog):path'', _) -> do
              (~(Just k), _) <- go' (crumbh, choiceName, crumbp<>"0") [] def
              fmap (first $ fmap Right)
                . go' (crumbh, choiceName, crumbp<>"1") (prog:path'') $ opt k
            (_, [[]], PositionAdvance) -> do
              (key', _) <- go' (crumbh,choiceName,crumbp<>"0") [[]] def
              case key' of
               Just k -> do
                 setProgress path k
                 skipContentless (crumbh, choiceName, crumbp<>"1") $ opt k
                 return (Nothing, True)
               Nothing -> error $ outerConstructorName def ++ " refuses to yield a result value."
            (_, [[]], PositionRevert) -> do
              revertProgress path
              lHasContent
                    <- hasDisplayableContent (crumbh, choiceName, crumbp<>"0") def
              return (Nothing, lHasContent)
            (Just k, [], PositionAdvance)
             -> fmap (first $ fmap Right)
                 . go' (crumbh, choiceName, crumbp<>"1") [] $ opt k
            (Nothing, [], PositionAdvance)
             -> return (Nothing, False)
            (_, dir:_, _)
             -> error $ "Div-ID "++dir++" not suitable for making a Dependent choice."
       go crumbs path (Styling _ cont) = go crumbs path cont
       go (crumbh, choiceName, _) (divid:path) (Encaps ManualCSSClasses ff (WriterT conts))
         | Just dividt <-  HTMDiv<$>Txt.stripPrefix "div." (Txt.pack divid)
                       <|> HTMSpan<$>Txt.stripPrefix "span." (Txt.pack divid)
            = let subSel = case lookup dividt $ swap<$>conts of
                   Just ssl -> ssl
                   _ -> error $ "Cannot lookup "++show dividt++" in "++show (snd<$>conts)
              in first (fmap $ ff . WriterT . pure . (,dividt))
                  <$> go (crumbh<>case dividt of
                                 HTMDiv i -> " div."<>i
                                 HTMSpan i -> " span."<>i
                                , choiceName, "") path subSel
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
                              -> IPresentation IO r
                              -> StateT PresProgress Handler (Maybe r)
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
            Just k -> fmap (fmap Right)
                       . skipContentless (crumbh, choiceName, crumbp<>"1") $ opt k
            Nothing -> do
               key' <- skipContentless (crumbh, choiceName, crumbp<>"0") def
               case key' of
                 Just k' -> do
                   setProgress thisDecision k'
                   fmap (fmap Right)
                    . skipContentless (crumbh, choiceName, crumbp<>"1") $ opt k'
                 Nothing -> return Nothing
       skipContentless crumbs (Styling _ c) = skipContentless crumbs c
       skipContentless crumbs (Resultless c)
           = fmap (const ()) <$> skipContentless crumbs c
       skipContentless crumbs (Deterministic f c)
           = (f=<<) <$> skipContentless crumbs c
       skipContentless _ (StaticContent _) = return Nothing
       skipContentless _ (DynamicContent _) = return Nothing
       skipContentless _ (Encaps _ _ _) = return Nothing
       skipContentless _ p = error
        $ "`skipContentless` does not support "++outerConstructorName p

       hasDisplayableContent :: (PrPath, Text->PrPath, Text)
                              -> IPresentation IO r
                              -> StateT PresProgress Handler Bool
       hasDisplayableContent _ (Pure _) = return False
       hasDisplayableContent crumbs (Interactive p _) = hasDisplayableContent crumbs p
       hasDisplayableContent (crumbh,choiceName,crumbp) (Dependent def opt) = do
          let thisDecision = crumbh <> " span."<>choiceName crumbp
          key <- lookupProgress thisDecision
          case key of
            Just k -> hasDisplayableContent (crumbh, choiceName, crumbp<>"1") $ opt k
            Nothing -> do
               hasDisplayableContent (crumbh, choiceName, crumbp<>"0") def
       hasDisplayableContent crumbs (Styling _ c) = hasDisplayableContent crumbs c
       hasDisplayableContent crumbs (Resultless c)
           = hasDisplayableContent crumbs c
       hasDisplayableContent crumbs (Deterministic f c)
           = hasDisplayableContent crumbs c
       hasDisplayableContent _ (StaticContent _) = return True
       hasDisplayableContent _ (DynamicContent _) = return True
       hasDisplayableContent _ (Encaps _ _ _) = return True
       hasDisplayableContent _ p = error
        $ "`hasDisplayableContent` does not support "++outerConstructorName p

       finePath p
        | Just prog <- fmap (Txt.dropWhile (=='n'))
                    $ Txt.stripPrefix "span."
                     =<< Txt.stripSuffix "slide" p
           = Txt.unpack prog
        | otherwise  = Txt.unpack p

defaultChoiceName :: Text->PrPath
defaultChoiceName pdiv = "n"<>pdiv<>"slide"

disambiguateChoiceName :: (Text->PrPath) -> (Text->PrPath)
disambiguateChoiceName = (.("n"<>))

getResetR :: Handler Html
getResetR = do
    clearSession
    redirect HomeR


class (MonadHandler m) => KnowsProgressState m where
  lookupProgress :: (HasCallStack, Flat x) => PrPath -> m (Maybe x)

instance MonadHandler m
            => KnowsProgressState (StateT PresProgress m) where
  lookupProgress path = get >>= lift . runReaderT (lookupProgress path)
instance MonadHandler m
            => KnowsProgressState (ReaderT PresProgress m) where
  lookupProgress path = do
   PresProgress progs <- ask
   case Map.lookup (Txt.words path) progs of
     Just bs
        | Right decoded <- unflat bs  -> return $ Just decoded
        | otherwise                   -> error $
            "Internal error in `lookupProgress`: value "++show bs++" cannot be decoded."
     Nothing -> return Nothing
 

setProgress :: (MonadHandler m, Flat x) => PrPath -> x -> StateT PresProgress m ()
setProgress path prog = do
   PresProgress progs <- get
   
   let progs' = Map.insert (Txt.words path)
                           (flat prog) progs

   put $ PresProgress progs'

revertProgress :: MonadHandler m => PrPath -> StateT PresProgress m Bool
revertProgress path = do
   PresProgress progs <- get
   let progs' = Map.delete (Txt.words path) progs
   put $ PresProgress progs'
                  
   return $ Txt.words path`Map.member`progs

     

data YeamerServerConfig = YeamerServerConfig
   { _yeamerTcpPort :: Int }
makeLenses ''YeamerServerConfig

instance Default YeamerServerConfig where
  def = YeamerServerConfig 14910
     
yeamer' :: YeamerServerConfig -> Presentation -> IO ()
yeamer' (YeamerServerConfig port) presentation = do
   createDirectoryIfMissing True pStatDir
   pStat <- static pStatDir
   warp port $ PresentationServer (preprocPres presentation) myStatic pStat

-- | Run a Yesod/Warp web server that will allow the presentation to be viewed
--   in a web browser, on port 14910. This is a shorthand for @'yeamer'' 'def'@.
yeamer :: Presentation -> IO ()
yeamer = yeamer' def



data DisplayOrientation = DisplayHorizontally | DisplayVertically
  deriving (Eq, Show, Generic)
otherDisplayOrientation :: DisplayOrientation -> DisplayOrientation
otherDisplayOrientation DisplayHorizontally = DisplayVertically
otherDisplayOrientation DisplayVertically = DisplayHorizontally

class InteractiveShow a where
  display :: a -> Presentation
  display = displayOriented DisplayHorizontally
  displayOriented :: DisplayOrientation -> a -> Presentation
  default displayOriented :: (Generic a, GInteractiveShow (Rep a))
              => DisplayOrientation -> a -> Presentation
  displayOriented orient = gDisplayOriented orient . from
  displayList :: DisplayOrientation -> [a] -> Presentation
  displayList orient = go 1
   where go _ [] = "[]"
         go n xs = case splitAt (sum [1..n]) xs of
           (l₀:r@(_:_),[])
               -> let m = init r; ω = last r
                  in foldr1 catOp $ ["["│display' l₀│" "]
                                 ++ map ((","│).(│" ").display') m
                                 ++ [","│display' ω│"]"]
           (l,[])
               -> "[" │ foldr1 (│) (display'<$>l) │ "]"
           (l₀:r@(_:_),_)
               -> (let m = init r; ω = last r
                   in foldr1 catOp $ ["("│display' l₀]
                                  ++ map ((":"│).display') m
                                  ++ [ ":"│display' ω
                                     , ":"│"...)" ]
                    ) >>= \() -> go (n + 1) xs
           (l,_) -> "(" │ foldr1 (│) (display'<$>l) │ ":" │ "..." │ ")"
                      >>= \() -> go (n + 1) xs
         catOp = case orient of
          DisplayHorizontally -> (│)
          DisplayVertically -> (──)
         display' = displayOriented $ otherDisplayOrientation orient

class GInteractiveShow f where
  gDisplayOriented :: DisplayOrientation -> f p -> Presentation

instance InteractiveShow Char where
  displayOriented _ c = fromString $ show c
  displayList _ s = fromString $ show s
instance InteractiveShow Int where displayOriented _ s = fromString $ show s
instance InteractiveShow Int64 where displayOriented _ s = fromString $ show s
instance InteractiveShow Int32 where displayOriented _ s = fromString $ show s
instance InteractiveShow Int16 where displayOriented _ s = fromString $ show s
instance InteractiveShow Double where
  displayOriented _ x
    | fromIntegral rounded == x   = display rounded
    | read imprecise == x         = fromString imprecise
    | otherwise = do
        approxShw
        fromString $ show x
   where rounded = round x :: Int16
         imprecise = printf "%.1g" x
         approxShw = case break (=='e') imprecise of
           (man,ex) -> fromString $ man ++ "…" ++ ex
instance InteractiveShow a => InteractiveShow [a] where
  displayOriented = displayList


instance GInteractiveShow V1 where
  gDisplayOriented _ a = case a of {}
instance GInteractiveShow U1 where
  gDisplayOriented _ _ = mempty
instance (GInteractiveShow l, GInteractiveShow r) => GInteractiveShow (l:+:r) where
  gDisplayOriented orient (L1 x) = gDisplayOriented orient x
  gDisplayOriented orient (R1 y) = gDisplayOriented orient y
instance (GInteractiveShow l, GInteractiveShow r) => GInteractiveShow (l:*:r) where
  gDisplayOriented orient (x:*:y) = gDisplayOriented orient x ⊕ gDisplayOriented orient y
   where (⊕) = case orient of DisplayHorizontally -> (│)
                              DisplayVertically -> (──)
instance InteractiveShow a => GInteractiveShow (K1 i a) where
  gDisplayOriented orient (K1 x)
        = displayOriented (otherDisplayOrientation orient) x

instance GInteractiveShow f => GInteractiveShow (M1 i ('MetaData ν μ π τ) f) where
  gDisplayOriented orient (M1 x) = dispDataEncapsulated $ gDisplayOriented orient x
instance KnownSymbol n
              => GInteractiveShow (M1 i ('MetaCons n φ σ) U1) where
  gDisplayOriented orient (M1 x) = dispConstructorLabel @n
instance (GInteractiveShow f, GInteractiveShow g, KnownSymbol n)
              => GInteractiveShow (M1 i ('MetaCons n φ σ) (f:*:g)) where
  gDisplayOriented orient (M1 x) = styling ([cassius|
                  .yeamer-display-dataFields-vert>.autogrid>div
                    border-top: 1px dashed
                  .yeamer-display-dataFields-horiz>.autogrid>div
                    border-left: 1px dashed
                                             |]())
          $ dispConstructorLabel @n
              <> divClass ("yeamer-display-dataFields-"<>rfOrient)
                     (gDisplayOriented orient x)
   where rfOrient = case orient of DisplayHorizontally -> "horiz"
                                   DisplayVertically -> "vert"
instance (GInteractiveShow (M1 j μ f), KnownSymbol n)
              => GInteractiveShow (M1 i ('MetaCons n φ σ)
                                        (M1 j μ f)) where
  gDisplayOriented orient (M1 x) = dispConstructorLabel @n
                    ── gDisplayOriented (otherDisplayOrientation orient
                                          -- single-field cons: undo K1 direction-change
                                        ) x

instance GInteractiveShow f
    => GInteractiveShow (M1 i ('MetaSel 'Nothing υ σ 'DecidedStrict) f) where
  gDisplayOriented orient (M1 x) = gDisplayOriented orient x
instance GInteractiveShow f
    => GInteractiveShow (M1 i ('MetaSel 'Nothing υ σ 'DecidedUnpack) f) where
  gDisplayOriented orient (M1 x) = gDisplayOriented orient x
instance GInteractiveShow f
    => GInteractiveShow (M1 i ('MetaSel 'Nothing υ σ 'DecidedLazy) f) where
  gDisplayOriented orient (M1 x) = "..." >>= \() -> gDisplayOriented orient x
instance (GInteractiveShow f, KnownSymbol n)
             => GInteractiveShow (M1 i ('MetaSel ('Just n) υ σ 'DecidedStrict) f) where
  gDisplayOriented orient (M1 x)
             = dispRecFieldLabel @n ⊕ gDisplayOriented orient x
   where (⊕) = case orient of DisplayHorizontally -> (──)
                              DisplayVertically -> (│)
instance (GInteractiveShow f, KnownSymbol n)
             => GInteractiveShow (M1 i ('MetaSel ('Just n) υ σ 'DecidedUnpack) f) where
  gDisplayOriented orient (M1 x)
             = dispRecFieldLabel @n ⊕ gDisplayOriented orient x
   where (⊕) = case orient of DisplayHorizontally -> (──)
                              DisplayVertically -> (│)
instance (GInteractiveShow f, KnownSymbol n)
             => GInteractiveShow (M1 i ('MetaSel ('Just n) υ σ 'DecidedLazy) f) where
  gDisplayOriented orient (M1 x)
    = (dispRecFieldLabel @n ⊕ "...")
       >>= \() -> (dispRecFieldLabel @n ⊕ gDisplayOriented orient x)
   where (⊕) = case orient of DisplayHorizontally -> (──)
                              DisplayVertically -> (│)

dispDataEncapsulated :: Presentation -> Presentation
dispDataEncapsulated = divClass "yeamer-display-dataEncapsulation"

dispConstructorLabel :: ∀ n . KnownSymbol n => Presentation
dispConstructorLabel = divClass "yeamer-display-dataConstructorName"
                      $ fromString (symbolVal @n Proxy)

dispRecFieldLabel :: ∀ n . KnownSymbol n => Presentation
dispRecFieldLabel = divClass "yeamer-display-recordFieldLabel"
                      $ fromString (symbolVal @n Proxy ++ "=")


newtype TableContents c = TableContents
  { getTableContents :: [[c]] }
 deriving (Functor, Traversable, Foldable, Flat)

table :: [IPresentation m ()]   -- ^ Headers
      -> [[IPresentation m ()]] -- ^ Cells
      -> IPresentation m ()
table headers cells = Encaps
   (CustomEncapsulation (EncapsulableWitness SessionableWitness)
     $ \(TableContents (h:cs)) -> HTM.table . HTM.tbody
          $ (case h of
              [] -> mempty
              hcs -> HTM.thead $ foldMap HTM.th hcs)
           <> HTM.tbody (
               foldMap (\row
                 -> HTM.tr $ foldMap HTM.td row
                 ) cs )
     )
   (const ())
   . TableContents $ headers : cells

