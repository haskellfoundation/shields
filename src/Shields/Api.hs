{-# LANGUAGE OverloadedStrings #-}

module Shields.Api (api) where

import Control.Applicative ((<|>))
import Jsonifier (toByteString)
import Shields.Response (parseResponse, renderResponse)
import Snap.Core
  ( Method (GET, HEAD),
    Snap,
    finishWith,
    getResponse,
    getsRequest,
    ifTop,
    method,
    methods,
    modifyResponse,
    path,
    rqQueryParams,
    setContentType,
    setResponseStatus,
    writeBS,
  )

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
      queryParams <- getsRequest rqQueryParams
      case parseResponse queryParams of
        Nothing -> do
          modifyResponse . setResponseStatus 500 $ "Invalid badge request"
          getResponse >>= finishWith
        Just sr -> do
          writeBS . toByteString . renderResponse $ sr
          resp <- getResponse
          finishWith . setContentType "application/json" $ resp
