{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.CaseInsensitive (mk)
import qualified Data.Map.Strict as M
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
    get,
    runHandler,
    setRequestPath,
    setRequestType,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Responses"
    [ testCase "Get, /heartbeat" $ do
        resp <- runHandler getHeartbeat api
        assertEqual "Status code is 200" 200 . rspStatus $ resp
        assertEqual "Status reason is 'Ok'" "Ok" . rspStatusReason $ resp,
      testCase "Head, /heartbeat" $ do
        resp <- runHandler headHeartbeat api
        assertEqual "Status code is 200" 200 . rspStatus $ resp
        assertEqual "Status reason is 'Ok'" "Ok" . rspStatusReason $ resp,
      testCase "Get, /, no query params" $ do
        resp <- runHandler getTopLevelNoQP api
        assertEqual "Status code is 500" 500 . rspStatus $ resp
        assertEqual "Explains that badge request is invalid" "Invalid badge request"
          . rspStatusReason
          $ resp,
      testCase "Get, /, label only" $ do
        resp <- runHandler getTopLevelLabelOnly api
        assertEqual "Status code is 200" 200 . rspStatus $ resp
        assertEqual "Content type is JSON" (Just "application/json")
          . getHeader (mk "content-type")
          $ resp
    ]

-- Helpers

getHeartbeat :: RequestBuilder IO ()
getHeartbeat = get "/heartbeat" M.empty

headHeartbeat :: RequestBuilder IO ()
headHeartbeat = do
  setRequestType . RequestWithRawBody HEAD $ ""
  setRequestPath "/heartbeat"

getTopLevelNoQP :: RequestBuilder IO ()
getTopLevelNoQP = get "/" M.empty

getTopLevelLabelOnly :: RequestBuilder IO ()
getTopLevelLabelOnly = get "/" . M.singleton "label" $ ["foo"]
