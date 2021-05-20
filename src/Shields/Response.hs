{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Shields.Response
  ( Colour,
    renderColour,
    parseColour,
    Style,
    renderStyle,
    parseStyle,
    Logo,
    renderLogo,
    parseLogo,
    LogoWidth,
    renderLogoWidth,
    parseLogoWidth,
    LogoPosition,
    renderLogoPosition,
    parseLogoPosition,
    Seconds,
    parseSeconds,
    renderSeconds,
    ShieldResponse (..),
    parseResponse,
    renderResponse,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Jsonifier
  ( Json,
    bool,
    intNumber,
    object,
    textString,
    writeJson,
  )
import PtrPoker.Write (Write, byteString)

-- | @since 1.0.0
newtype Colour = Colour Write

-- | @since 1.0.0
renderColour :: Colour -> Json
renderColour = writeJson . coerce

-- | @since 1.0.0
parseColour :: ByteString -> Colour
parseColour = Colour . quote

-- | @since 1.0.0
newtype Style = Style Write

-- | @since 1.0.0
renderStyle :: Style -> Json
renderStyle = writeJson . coerce

-- | @since 1.0.0
parseStyle :: ByteString -> Style
-- Best-effort guess. - Koz
parseStyle = Style . quote

-- | @since 1.0.0
newtype Logo = Logo Write

-- | @since 1.0.0
renderLogo :: Logo -> Json
renderLogo = writeJson . coerce

-- | @since 1.0.0
parseLogo :: ByteString -> Logo
parseLogo = Logo . quote

-- | @since 1.0.0
newtype LogoWidth = LogoWidth Write

-- | @since 1.0.0
renderLogoWidth :: LogoWidth -> Json
renderLogoWidth = writeJson . coerce

-- | @since 1.0.0
parseLogoWidth :: ByteString -> LogoWidth
-- No quoting needed, it's a number, or should be. - Koz
parseLogoWidth = LogoWidth . byteString

-- | @since 1.0.0
newtype LogoPosition = LogoPosition Write

-- | @since 1.0.0
renderLogoPosition :: LogoPosition -> Json
renderLogoPosition = writeJson . coerce

-- | @since 1.0.0
parseLogoPosition :: ByteString -> LogoPosition
-- Best-effort guess. - Koz
parseLogoPosition = LogoPosition . quote

-- | @since 1.0.0
newtype Seconds = Seconds Write

-- | @since 1.0.0
renderSeconds :: Seconds -> Json
renderSeconds = writeJson . coerce

-- | @since 1.0.0
parseSeconds :: ByteString -> Seconds
-- No quoting, since this should be a number. - Koz
parseSeconds = Seconds . byteString

-- | @since 1.0.0
data ShieldResponse = ShieldResponse
  { -- | @since 1.0.0
    label :: {-# UNPACK #-} !Write,
    -- | @since 1.0.0
    colour :: !(Maybe Colour),
    -- | @since 1.0.0
    labelColour :: !(Maybe Colour),
    -- | @since 1.0.0
    logoColour :: !(Maybe Colour),
    -- | @since 1.0.0
    namedLogo :: !(Maybe Logo),
    -- | @since 1.0.0
    logoSvg :: !(Maybe Logo),
    -- | @since 1.0.0
    logoWidth :: !(Maybe LogoWidth),
    -- | @since 1.0.0
    logoPosition :: !(Maybe LogoPosition),
    -- | @since 1.0.0
    style :: !(Maybe Style),
    -- | @since 1.0.0
    cacheSeconds :: !(Maybe Seconds)
  }

-- | @since 1.0.0
parseResponse :: Map ByteString [ByteString] -> Maybe ShieldResponse
parseResponse queryParams = do
  singular <- traverse ensureSingle queryParams
  label' <- quote <$> M.lookup "label" singular
  pure
    . ShieldResponse
      label'
      (parseColour <$> M.lookup "color" singular)
      (parseColour <$> M.lookup "labelColor" singular)
      (parseColour <$> M.lookup "logoColor" singular)
      (parseLogo <$> M.lookup "namedLogo" singular)
      (parseLogo <$> M.lookup "logoSvg" singular)
      (parseLogoWidth <$> M.lookup "logoWidth" singular)
      (parseLogoPosition <$> M.lookup "logoPosition" singular)
      (parseStyle <$> M.lookup "style" singular)
    $ (parseSeconds <$> M.lookup "cacheSeconds" singular)

-- | @since 1.0.0
renderResponse :: ShieldResponse -> Json
renderResponse sr =
  object . catMaybes $
    [ pure ("schemaVersion", intNumber 1),
      pure ("label", writeJson . label $ sr),
      pure ("message", textString "Haskell Foundation"),
      ("color",) . renderColour <$> colour sr,
      ("labelColor",) . renderColour <$> labelColour sr,
      pure ("isError", bool False),
      ("namedLogo",) . renderLogo <$> namedLogo sr,
      ("logoSvg",) . renderLogo <$> logoSvg sr,
      ("logoColor",) . renderColour <$> logoColour sr,
      ("logoWidth",) . renderLogoWidth <$> logoWidth sr,
      ("logoPosition",) . renderLogoPosition <$> logoPosition sr,
      ("style",) . renderStyle <$> style sr,
      ("cacheSeconds",) . renderSeconds <$> cacheSeconds sr
    ]

-- Helpers

ensureSingle :: [ByteString] -> Maybe ByteString
ensureSingle = \case
  [x] -> pure x
  _ -> Nothing

quote :: ByteString -> Write
quote bs = "\"" <> byteString bs <> "\""
