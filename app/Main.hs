module Main (main) where

import Shields.Api (api)
import Snap.Http.Server (defaultConfig, httpServe)

main :: IO ()
main = httpServe defaultConfig api
