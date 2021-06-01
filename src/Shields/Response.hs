{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Shields.Response
  ( ShieldResponse (..),
    renderResponse,
    defaultResponse,
  )
where

import Data.Number.Scientific (fromWord64)
import Data.Text.Short (ShortText)
import Json
  ( Value (Number, String),
    object3,
    pattern (:->),
  )

-- | @since 1.0.0
newtype ShieldResponse = ShieldResponse
  { -- | @since 1.0.0
    label :: ShortText
  }
  deriving
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Show
    )
    via ShortText

-- | @since 1.0.0
defaultResponse :: ShieldResponse
defaultResponse = ShieldResponse ""

-- | @since 1.0.0
renderResponse :: ShieldResponse -> Value
renderResponse (ShieldResponse l) =
  object3
    ("schemaVersion" :-> (Number . fromWord64 $ 1))
    ("label" :-> String l)
    ("message" :-> String "Haskell Foundation")
