{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.CaseInsensitive (mk)
import Shields.Api (api)
import Snap.Core
  ( Method (HEAD),
    getHeader,
    rspStatus,
    rspStatusReason,
  )
import Snap.Test
  ( RequestBuilder,
    RequestType (RequestWithRawBody),
    assertSuccess,
    get,
    runHandler,
    setRequestPath,
    setRequestType,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Prelude hiding (lookup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Responses"
    [ testCase "Get, /heartbeat" $ do
        resp <- runHandler getHeartbeat api
        assertSuccess resp
        assertEqual "Status reason is 'Ok'" "Ok" . rspStatusReason $ resp,
      testCase "Head, /heartbeat" $ do
        resp <- runHandler headHeartbeat api
        assertSuccess resp
        assertEqual "Status reason is 'Ok'" "Ok" . rspStatusReason $ resp,
      testCase "Get, /" $ do
        resp <- runHandler getTop api
        assertEqual "Status code is 200" 200 . rspStatus $ resp
        assertEqual "Content type is JSON" (Just "application/json") . getHeader (mk "Content-Type") $ resp
    ]

-- Helpers

getTop :: RequestBuilder IO ()
getTop = get "/" mempty

getHeartbeat :: RequestBuilder IO ()
getHeartbeat = get "/heartbeat" mempty

headHeartbeat :: RequestBuilder IO ()
headHeartbeat = do
  setRequestType . RequestWithRawBody HEAD $ ""
  setRequestPath "/heartbeat"
