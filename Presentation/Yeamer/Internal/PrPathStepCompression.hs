-- |
-- Module      : Presentation.Yeamer.Internal.PrPathStepCompression
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 
module Presentation.Yeamer.Internal.PrPathStepCompression where


import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as Txt

compressPrPathSteps :: [Txt.Text] -> BS.ByteString
compressPrPathSteps
    = BSL.toStrict . Zlib.compress . BSL.fromStrict . Txt.encodeUtf8 . Txt.unlines
decompressPrPathSteps :: BS.ByteString -> [Txt.Text]
decompressPrPathSteps
    = Txt.lines . Txt.decodeUtf8 . BSL.toStrict . Zlib.decompress . BSL.fromStrict

noProgressSteps :: BS.ByteString
noProgressSteps = compressPrPathSteps []
