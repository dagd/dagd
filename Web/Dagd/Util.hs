{-# LANGUAGE OverloadedStrings #-}

module Web.Dagd.Util where

import Control.Applicative

import Data.List (isInfixOf)
import qualified Data.Text.Lazy as T

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Web.Scotty

safe :: ActionM a -> ActionM (Maybe a)
safe = (`rescue` const (return Nothing)) . (Just `fmap`)

paramMay :: (Parsable a) => T.Text -> ActionM (Maybe a)
paramMay = safe . param

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
