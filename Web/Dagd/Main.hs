{-# LANGUAGE OverloadedStrings #-}

module Web.Dagd.Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend, mconcat)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as TE

import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Network.Whois
import qualified Network.Socket as S

import Graphics.ImageMagick.MagickWand

import Web.Dagd.Util
import Web.Scotty

main = scotty 3000 $ do
  middleware $ gzip $ def { gzipFiles = GzipCompress }
  middleware logStdoutDev

  get "/ip" $ do
    ip <-  fmap (T.pack . init . dropWhileEnd (/= ':') . show . remoteHost) request
    prepareResponse ip

  get "/ua" $ do
    agent <- reqHeader "User-Agent"
    maybe (raise "User-Agent header not found!") prepareResponse agent

  get "/w/:query" $ do
    query <- param "query"
    x <- liftIO $ whois query
    prepareResponse $ T.pack . unlines $ fmap (fromMaybe "") [fst x, snd x]

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
