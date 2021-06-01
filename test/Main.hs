{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Bytes (fromByteString)
import Data.CaseInsensitive (mk)
import Data.Foldable (find)
import Data.Number.Scientific (fromWord64)
import Json
  ( Member (Member),
    Value (Number, Object, String),
    decode,
    value,
  )
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
    getResponseBody,
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
        parsed <- decode . fromByteString <$> getResponseBody resp
        assertEqual
          "Content is a three-attribute object"
          (Right . Just $ 3)
          (checkObject <$> parsed)
        assertEqual
          "schemaVersion attribute has value 1"
          (Right . Just . Number . fromWord64 $ 1)
          (checkSchemaVersion <$> parsed)
        assertEqual
          "label attribute has value ''"
          (Right . Just . String $ "")
          (checkLabel <$> parsed)
        assertEqual
          "message attribute has value 'Haskell Foundation'"
          (Right . Just . String $ "Haskell Foundation")
          (checkMessage <$> parsed)
    ]

-- Helpers

checkObject :: Value -> Maybe Int
checkObject = \case
  Object mems -> Just . length $ mems
  _ -> Nothing

checkSchemaVersion :: Value -> Maybe Value
checkSchemaVersion = \case
  Object mems -> value <$> find go mems
  _ -> Nothing
  where
    go :: Member -> Bool
    go (Member k _) = k == "schemaVersion"

checkLabel :: Value -> Maybe Value
checkLabel = \case
  Object mems -> value <$> find go mems
  _ -> Nothing
  where
    go :: Member -> Bool
    go (Member k _) = k == "label"

checkMessage :: Value -> Maybe Value
checkMessage = \case
  Object mems -> value <$> find go mems
  _ -> Nothing
  where
    go :: Member -> Bool
    go (Member k _) = k == "message"

getTop :: RequestBuilder IO ()
getTop = get "/" mempty

getHeartbeat :: RequestBuilder IO ()
getHeartbeat = get "/heartbeat" mempty

headHeartbeat :: RequestBuilder IO ()
headHeartbeat = do
  setRequestType . RequestWithRawBody HEAD $ ""
  setRequestPath "/heartbeat"
