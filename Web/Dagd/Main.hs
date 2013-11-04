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

import Text.Blaze (toValue)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as HA
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

  get (regex "/([[:alnum:]]+)/?(.+)?") $ do
    shorturl <- param "1" :: ActionM String
    suffix <- paramMay "2"
    result <- liftIO $
      query db "select * from shorturls where shorturl=?" (Only shorturl)
    if null result
      then next
      else do
        let s = fromMaybe "" (T.append "/" <$> suffix)
        qs <- fmap (T.fromStrict . TE.decodeUtf8 . rawQueryString) request
        redirect $ (T.pack . TS.unpack . urlLongurl $ head result) `mappend` s `mappend` qs

  get "/" $ do
    prepareResponseHtml $ do
      H.b "da.gd"
      mconcat $ replicate 2 H.br
      H.form ! HA.action "/" ! HA.method "post" $ do
        "Long URL: "
        H.input ! HA.type_ "text" ! HA.name "url" ! HA.id "url" ! HA.size "35"
        mconcat $ replicate 2 H.br
        "Optional custom suffix (truncated at 10 chars): "
        H.input ! HA.type_ "text" ! HA.name "shorturl" ! HA.size "20"
        mconcat $ replicate 2 H.br
        H.input ! HA.type_ "submit" ! HA.value "Shorten URL"
        mconcat $ replicate 2 H.br
        H.a ! HA.href "/help" $ "help"
        " | "
        H.a ! HA.href "https://github.com/codeblock/dagd" $ "open source"
      -- Add this after the form is drawn to ensure the form exists first
      H.script $ "document.getElementById('url').focus();"

  post "/" $ do
    url <- param "url" :: ActionM String
    shorturl <- param "shorturl" :: ActionM String
    ip <- fmap (init . dropWhileEnd (/= ':') . show . remoteHost) request

    -- This is conditional-hell and should be refactored at some point.
    -- It's also rather inefficient.
    if not $ isValidLongURL url
      then do
        status badRequest400
        prepareResponse $ "Invalid long URL given."
      else do
        if (shorturl /= "") && (not $ isValidShortUrl shorturl)
          then do
            status badRequest400
            prepareResponse $ "Invalid short URL given. Valid characters: "
              `mappend` (T.pack possibleShortUrlChars)
          else do
            freeUrl <- liftIO $ isFreeShortUrl db shorturl
            if (shorturl /= "") && (not $ freeUrl)
              then do
                status badRequest400
                prepareResponse $ "That short URL was already taken!"
              else do
                s <- liftIO $ shortUrl shorturl url db
                liftIO $
                  execute db ("insert into shorturls("
                    `mappend` "shorturl, longurl, owner_ip, enabled, "
                    `mappend` "custom_shorturl) values(?, ?, ?, ?, ?)") $
                  ShortUrl Nothing s (TS.pack url) ip Nothing True (shorturl /= "")

                prepareResponseHtml $
                  H.a ! HA.href ("http://da.gd/" `mappend` (toValue s)) $
                    ("http://da.gd/" `mappend` (H.toHtml s))

  get "/c" $ do
    result <- liftIO $
      query_ db "select * from command_redirects where enabled=true;"
        :: ActionT IO [Command]
    let html = H.b "Enabled Commands" : map H.toHtml result
    prepareResponseHtml $ mconcat html
