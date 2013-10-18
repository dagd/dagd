{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mconcat)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as TE

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Network.Whois
import qualified Network.Socket as S

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Scotty

isTextUseragent :: Maybe String -> Bool
isTextUseragent (Just a) = any (`isInfixOf` a) textUAs
  where
    textUAs = ["Wget"
             , "curl"
             , "libcurl"
             , "Supybot"
             , "Ruby"
             , "NetBSD-ftp"
             , "HTTPie"
             , "OpenBSD ftp"
             , "haskell-HTTP"
             ]
isTextUseragent Nothing = False

prepareResponse :: T.Text -> ActionM ()
prepareResponse a = do
  agent <- reqHeader "User-Agent"
  if isTextUseragent $ T.unpack <$> agent
           then text a
           else html $ renderHtml $
             H.html $
               H.body $
                 H.pre $ H.toHtml a

main = scotty 3000 $ do
  middleware $ gzip $ def { gzipFiles = GzipCompress }
  middleware logStdoutDev

  get "/ip" $ do
    ip <- fmap (T.pack . show . remoteHost) request
    prepareResponse ip

  get "/ua" $ do
    agent <- reqHeader "User-Agent"
    maybe (raise "User-Agent header not found!") prepareResponse agent

  get "/w/:query" $ do
    query <- param "query"
    x <- liftIO $ whois query
    prepareResponse $ T.pack . unlines $ fmap (fromMaybe "") x

  get "/status/:code/:message" $ do
    code <- param "code"
    message <- param "message"
    status $ Status code message

  get "/et/:item" $ do
    item <- param "item"
    qs <- fmap (T.fromStrict . TE.decodeUtf8 . rawQueryString) request
    redirect $ "http://www.etsy.com/listing/" `mappend` item `mappend` qs
