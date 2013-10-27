{-# LANGUAGE OverloadedStrings #-}
module Web.Dagd.Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import Data.List (dropWhileEnd, intercalate, nub)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mconcat)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as TE

import Database.PostgreSQL.Simple

import Graphics.ImageMagick.MagickWand

import Network.HTTP.Conduit as NHC
import Network.HTTP.Types (notFound404)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Network.Whois hiding (query)
import qualified Network.Socket as S

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Dagd.DB.Command
import Web.Dagd.DB.ShortUrl
import Web.Dagd.Util
import Web.Scotty
import Web.Scotty.Trans (ActionT)

main = scotty 3000 $ do
  middleware $ gzip $ def { gzipFiles = GzipCompress }
  middleware logStdoutDev

  -- TODO: Move to a config file.
  -- No, this isn't the prod password ;)
  db <- liftIO $
    connect defaultConnectInfo { connectHost     = "localhost"
                               , connectDatabase = "dagd"
                               , connectUser     = "dagd"
                               , connectPassword = "dagdpassword" }

  get "/ip" $ do
    ip <- fmap (T.pack . init . dropWhileEnd (/= ':') . show . remoteHost) request
    prepareResponse ip

  get "/ua" $ do
    agent <- reqHeader "User-Agent"
    maybe (raise "User-Agent header not found!") prepareResponse agent

  get "/w/:query" $ do
    query <- param "query"
    x <- liftIO $ whois query
    prepareResponse $ T.pack . unlines $ fmap (fromMaybe "") [fst x, snd x]

  get "/host/:ip" $ do
    ip <- param "ip"
    unless (isIpAddress ip) next
    h <- liftIO $ fmap (S.addrAddress . head) $
      S.getAddrInfo Nothing (Just ip) Nothing
    name <- liftIO $ fmap fst $ S.getNameInfo [] True False h
    prepareResponse $ T.pack (fromMaybe "Unable to determine reverse DNS." name)

  get "/host/:host" $ do
    host <- param "host"
    results <- liftIO $
      fmap (fmap (init . dropWhileEnd (/= ':') . show . S.addrAddress)) $
        S.getAddrInfo Nothing (Just host) Nothing
    prepareResponse $ T.pack (intercalate ", " (nub results))

  get "/headers" $ do
    r <- request
    prepareResponse $ showHeaders (Network.Wai.requestHeaders r)

  get (regex "^/headers/(.*)$") $ do
    site <- param "1"
    rsp <- liftIO $ do
      u <- NHC.parseUrl site
      NHC.withManager $ NHC.httpLbs u
    prepareResponse $ showHeaders (NHC.responseHeaders rsp)

  get "/status/:code/:message" $ do
    code <- param "code"
    message <- param "message"
    status $ Status code message

  get "/et/:item" $ do
    item <- param "item"
    qs <- fmap (T.fromStrict . TE.decodeUtf8 . rawQueryString) request
    redirect $ "http://www.etsy.com/listing/" `mappend` item `mappend` qs

  get (regex "^/image/([0-9]+)[x*]([0-9]+)\\.([[:alnum:]]+)$") $ do
    width <- param "1"
    height <- param "2"
    extension <- param "3" :: ActionM String
    bgColor <- paramMay "bgcolor"
    text <- paramMay "text"

    img <- liftIO $ withMagickWandGenesis $ do
      (_,w) <- magickWand
      (_,dw) <- drawingWand
      c <- pixelWand
      c `setColor` BC8.append (BC8.pack "#") (fromMaybe "333333" bgColor)
      newImage w width height c
      -- Texty stuff.
      c `setColor` "white"
      dw `setFillColor` c
      dw `setTextAntialias` True
      dw `setStrokeOpacity` 0

      let m = fromMaybe (show width ++ "x" ++ show height) text
      drawAnnotation dw 65 65 (TS.pack m)

      -- Do housekeeping and return the image.
      drawImage w dw
      w `setImageFormat` TS.pack extension
      getImageBlob w

    mime <- liftIO $ withMagickWandGenesis $ toMime (TS.pack extension)
    setHeader "Content-Type" (T.fromStrict mime)
    raw $ BL.fromStrict img

  get "/:shorturl" $ do
    shorturl <- param "shorturl" :: ActionM String
    result <- liftIO $
      query db "select * from shorturls where shorturl=?" (Only shorturl)
    if null result
    then
      next
    else
      redirect (T.pack . TS.unpack . urlLongurl $ head result)

  get "/c" $ do
    result <- liftIO $
      query_ db "select * from command_redirects where enabled=true;"
        :: ActionT IO [Command]
    let html = H.b "Enabled Commands" : map H.toHtml result
    prepareResponseHtml $ mconcat html
