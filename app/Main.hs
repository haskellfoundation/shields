module Main (main) where

import Shields.Api (api)
import Snap.Http.Server
  ( commandLineConfig,
    emptyConfig,
    httpServe,
  )

main :: IO ()
main = commandLineConfig emptyConfig >>= flip httpServe api
