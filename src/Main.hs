{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T
import qualified Network.Socket as S

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Network.Whois

import Web.Scotty

main = scotty 3000 $ do
  middleware $ gzip $ def { gzipFiles = GzipCompress }
  middleware logStdoutDev

  get "/ip" $ do
    ip <- fmap (T.pack . show . remoteHost) request
    text ip

  get "/ua" $ do
    agent <- reqHeader "User-Agent"
    maybe (raise "User-Agent header not found!") text agent

  get "/w/:query" $ do
    query <- param "query"
    x <- liftIO $ whois query
    text $ T.pack . unlines $ fmap (fromMaybe "") x
