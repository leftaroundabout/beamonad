-- |
-- Module      : Presentation.Yeamer.Internal.Progress
-- Copyright   : (c) Justus Sagemüller 2018
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 
module Presentation.Yeamer.Internal.Progress where


import Presentation.Yeamer.Internal.PrPathStepCompression

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as Arr

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Txt
import qualified Data.ByteString.Base64.URL as URLBase64

import Data.Flat (flat, unflat)

import Yesod (PathPiece(..))

import Control.Arrow ((>>>), (<<<))
import Control.Monad ((>=>))
import Control.Monad.Trans.List
import Control.Monad.Trans.Writer
import Lens.Micro (_Right)
import Lens.Micro.Extras (preview)

import Data.Traversable.Redundancy (rmRedundancy)


newtype PresProgress = PresProgress
    { getPresentationProgress :: Map [Text] ByteString }
    deriving (Eq, Show, Read)

instance PathPiece PresProgress where
  fromPathPiece = Txt.encodeUtf8
              >>> preview _Right . URLBase64.decode
              >=> preview _Right . fmap assemblePresProgress . unflat
  toPathPiece   = Txt.decodeUtf8
              <<<                  URLBase64.encode
              <<<                  flat . disassemblePresProgress

assemblePresProgress :: ((ByteString, [ByteString]), Map [Int] Int) -> PresProgress
assemblePresProgress ((pSR_l_c, pKR_l), prog_c)
          = PresProgress . Map.mapKeys (map (progStepRsr Arr.!))
                          $ fmap (progKeyRsr Arr.!) prog_c
 where progStepRsr = Arr.fromList $ decompressPrPathSteps pSR_l_c
       progKeyRsr = Arr.fromList pKR_l

disassemblePresProgress :: PresProgress -> ((ByteString, [ByteString]), Map [Int] Int)
disassemblePresProgress (PresProgress progs)
         = ( ( compressPrPathSteps $ Arr.toList progStepRsr
             , Arr.toList progKeyRsr )
           , compressedProgs )
 where (ListT (WriterT keyCompressed), progStepRsr)
                  = rmRedundancy . ListT . WriterT $ Map.toList progs
       (compressedProgs,progKeyRsr) = rmRedundancy $ Map.fromList keyCompressed
