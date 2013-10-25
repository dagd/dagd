{-# LANGUAGE OverloadedStrings #-}
module Web.Dagd.TestApp where

import Network.HTTP.Types.Method

import Web.Dagd.Util
import Web.Scotty.Trans

render :: ScottyT IO ()
render =
  get "/test" $ do
    prepareResponse "I work!"
