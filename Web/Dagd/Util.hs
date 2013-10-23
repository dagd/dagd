{-# LANGUAGE OverloadedStrings #-}

module Web.Dagd.Util where

import Control.Monad
import Control.Applicative

import qualified Data.ByteString.Char8 as BC8
import qualified Data.CaseInsensitive as CI
import Data.Monoid (mappend)
import Data.List (isInfixOf)
import qualified Data.Text.Lazy as T

import Network.HTTP.Types.Header
import Network.URI (isIPv4address, isIPv6address)

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Scotty

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

prepareResponse :: T.Text -> ActionM ()
prepareResponse a = do
  agent <- reqHeader "User-Agent"
  if isTextUseragent $ T.unpack <$> agent
           then text a
           else html $ renderHtml $
             H.html $
               H.body $
                 H.pre $ H.toHtml a

isIpAddress :: String -> Bool
isIpAddress = liftM2 (||) isIPv4address isIPv6address
