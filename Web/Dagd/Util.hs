{-# LANGUAGE OverloadedStrings #-}

module Web.Dagd.Util where

import Control.Conditional (ifM)
import Control.Monad
import Control.Applicative

import qualified Data.ByteString.Char8 as BC8
import qualified Data.CaseInsensitive as CI
import Data.Monoid (mappend)
import Data.List (isInfixOf)
import qualified Data.Text.Lazy as T

import Network.HTTP.Types.Header
import Network.URI (isIPv4address, isIPv6address)

import Text.Blaze (Markup)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Scotty.Trans

safe :: ActionM a -> ActionM (Maybe a)
safe = (`rescue` const (return Nothing)) . (Just `fmap`)

paramMay :: (Parsable a) => T.Text -> ActionM (Maybe a)
paramMay = safe . param

showHeaders :: ResponseHeaders -> T.Text
showHeaders x = T.pack . unlines $
  fmap (\(a, b) ->
    BC8.unpack $ CI.original a `mappend` ": " `mappend` b) x

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

wantsPlainText :: ActionT IO Bool
wantsPlainText = do
  -- If given ?text, do what that says.
  -- Otherwise, check the useragent. If it's textual, return true
  textP <- paramMay "text" :: ActionM (Maybe String)
  case textP of
    Just p -> return $ if p == "0" || p == "false" then False else True
    Nothing -> do
      agent <- reqHeader "User-Agent"
      return $ isTextUseragent $ T.unpack <$> agent

prepareResponse :: T.Text -> ActionM ()
prepareResponse = wrapPrepareResponse . Left

prepareResponseHtml :: Markup -> ActionM ()
prepareResponseHtml = wrapPrepareResponse . Right

wrapPrepareResponse :: Either T.Text Markup -> ActionM ()
wrapPrepareResponse content = do
  --x <- wantsPlainText
  ifM wantsPlainText (text genText) (html $ renderHtml genHtml)
  where
    genHtml =
      H.docTypeHtml $ do
        H.head $ do
          H.title "da.gd"
          H.meta H.! HA.charset "utf8"
        H.body $
          case content of
            Left a -> H.toHtml a
            Right a -> a

    genText =
      case content of
        Left a -> a
        Right a -> renderHtml a

isIpAddress :: String -> Bool
isIpAddress = liftM2 (||) isIPv4address isIPv6address
