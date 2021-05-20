{-# LANGUAGE OverloadedStrings #-}

module Shields.Api (api) where

import Jsonifier (toByteString)
import Shields.Response (parseResponse, renderResponse)
import Snap.Core
  ( Snap,
    finishWith,
    getResponse,
    getsRequest,
    ifTop,
    modifyResponse,
    rqQueryParams,
    setContentType,
    setResponseStatus,
    writeBS,
  )

-- | @since 1.0.0
api :: Snap ()
api = ifTop $ do
  queryParams <- getsRequest rqQueryParams
  case parseResponse queryParams of
    Nothing -> do
      modifyResponse . setResponseStatus 500 $ "Invalid badge request"
      getResponse >>= finishWith
    Just sr -> do
      writeBS . toByteString . renderResponse $ sr
      resp <- getResponse
      finishWith . setContentType "application/json" $ resp
