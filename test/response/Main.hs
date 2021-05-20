{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.CaseInsensitive (mk)
import qualified Data.Map.Strict as M
import Shields.Api (api)
import Snap.Core (getHeader, rspStatus, rspStatusReason)
import Snap.Test (RequestBuilder, get, runHandler)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Responses"
    [ testCase "Get, /, no query params" $ do
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

getTopLevelNoQP :: RequestBuilder IO ()
getTopLevelNoQP = get "/" M.empty

getTopLevelLabelOnly :: RequestBuilder IO ()
getTopLevelLabelOnly = get "/" . M.singleton "label" $ ["foo"]
