{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Jsonifier (toByteString)
import Shields (parseResponse, renderResponse)
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
import Snap.Http.Server (defaultConfig, httpServe)

main :: IO ()
main = httpServe defaultConfig api

-- Helpers

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
