{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Shields.Response
  ( Colour,
    renderColour,
    Style,
    renderStyle,
    Logo,
    renderLogo,
    LogoWidth,
    renderLogoWidth,
    LogoPosition,
    renderLogoPosition,
    Seconds,
    renderSeconds,
    ShieldResponse (..),
    renderResponse,
    defaultResponse,
  )
where

import Data.Coerce (coerce)
import Data.Maybe (catMaybes)
import Jsonifier
  ( Json,
    bool,
    intNumber,
    object,
    textString,
    writeJson,
  )
import PtrPoker.Write (Write)

-- | @since 1.0.0
newtype Colour = Colour Write

-- | @since 1.0.0
renderColour :: Colour -> Json
renderColour = writeJson . coerce

-- | @since 1.0.0
newtype Style = Style Write

-- | @since 1.0.0
renderStyle :: Style -> Json
renderStyle = writeJson . coerce

-- | @since 1.0.0
newtype Logo = Logo Write

-- | @since 1.0.0
renderLogo :: Logo -> Json
renderLogo = writeJson . coerce

-- | @since 1.0.0
newtype LogoWidth = LogoWidth Write

-- | @since 1.0.0
renderLogoWidth :: LogoWidth -> Json
renderLogoWidth = writeJson . coerce

-- | @since 1.0.0
newtype LogoPosition = LogoPosition Write

-- | @since 1.0.0
renderLogoPosition :: LogoPosition -> Json
renderLogoPosition = writeJson . coerce

-- | @since 1.0.0
newtype Seconds = Seconds Write

-- | @since 1.0.0
renderSeconds :: Seconds -> Json
renderSeconds = writeJson . coerce

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

defaultResponse :: ShieldResponse
defaultResponse =
  ShieldResponse
    { label = mempty,
      colour = Nothing,
      labelColour = Nothing,
      logoColour = Nothing,
      namedLogo = Nothing,
      logoSvg = Nothing,
      logoWidth = Nothing,
      logoPosition = Nothing,
      style = Nothing,
      cacheSeconds = Nothing
    }

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
