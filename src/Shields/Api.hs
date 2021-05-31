{-# LANGUAGE OverloadedStrings #-}

module Shields.Api (api) where

import Control.Applicative ((<|>))
import Data.Binary.Builder (Builder, fromByteString)
import Jsonifier (toByteString)
import Shields.Response (defaultResponse, renderResponse)
import Snap.Core
  ( Method (GET, HEAD),
    Snap,
    finishWith,
    getResponse,
    ifTop,
    method,
    methods,
    modifyResponse,
    path,
    setContentType,
    setResponseBody,
    setResponseStatus,
  )
import System.IO.Streams (OutputStream)
import qualified System.IO.Streams as Streams

-- | @since 1.0.0
api :: Snap ()
api = heartbeat <|> shield
  where
    heartbeat :: Snap ()
    heartbeat = methods [GET, HEAD] . path "heartbeat" $ do
      modifyResponse . setResponseStatus 200 $ "Ok"
      getResponse >>= finishWith
    shield :: Snap ()
    shield = method GET . ifTop $ do
      modifyResponse . setResponseStatus 200 $ ""
      modifyResponse . setContentType $ "application/json"
      modifyResponse . setResponseBody $ go
      getResponse >>= finishWith
    go :: OutputStream Builder -> IO (OutputStream Builder)
    go stream = do
      Streams.writeTo stream
        . Just
        . fromByteString
        . toByteString
        . renderResponse
        $ defaultResponse
      pure stream
