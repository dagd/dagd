{-# LANGUAGE OverloadedStrings #-}
module Web.Dagd.Util where

import Control.Applicative
import Control.Conditional (ifM)
import Control.Monad

import Database.PostgreSQL.Simple

import qualified Data.ByteString.Char8 as BC8
import qualified Data.CaseInsensitive as CI
import Data.Monoid (mappend)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text.Lazy as T

import Network.HTTP.Types.Header
import Network.URI (isAbsoluteURI, isIPv4address, isIPv6address)

import System.Random

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
wrapPrepareResponse c = do
  ifM wantsPlainText (text genText) (html $ renderHtml genHtml)
  where
    genHtml =
      H.docTypeHtml $ do
        H.head $ do
          H.title "da.gd"
          H.meta H.! HA.charset "utf8"
        H.body $
          case c of
            Left a -> H.toHtml a
            Right a -> a

    genText =
      case c of
        Left a -> a
        Right a -> renderHtml a

isIpAddress :: String -> Bool
isIpAddress = liftM2 (||) isIPv4address isIPv6address

possibleShortUrlChars :: [Char]
possibleShortUrlChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-', '_']

isValidShortUrl :: String -> Bool
isValidShortUrl = all (flip elem possibleShortUrlChars)

isValidLongURL :: String -> Bool
isValidLongURL = liftM2 (&&) (isPrefixOf "http") isAbsoluteURI

pickRandomShortUrl :: Connection -> IO String
pickRandomShortUrl d = r False (return "")
  where
    r :: Bool -> IO String -> IO String
    r True x = x
    r False _ = do
      z <- randomShortUrl
      [Only c] <-
        query d "select count(*) from shorturls where shorturl=?"
          (Only z) :: IO [Only Int]
      r (c == 0) (return z)

    randomShortUrl :: IO String
    randomShortUrl =
      flip replicateM (pick possibleShortUrlChars) =<< randomRIO (2,5)
      where
        pick xs = liftM (xs !!) (randomRIO (0, length xs - 1))

getNonCustomShortUrl :: Connection -> String -> IO (Maybe String)
getNonCustomShortUrl d z = do
  q <- query d "select shorturl from shorturls where longurl=? and custom_shorturl=false" (Only z) :: IO [Only String]
  if null q
    then return Nothing
    else return $ Just (fromOnly (head q))

isFreeShortUrl :: Connection -> String -> IO Bool
isFreeShortUrl d z = do
  [Only c] <- query d "select count(*) from shorturls where shorturl=?" (Only z) :: IO [Only Int]
  return (c == 0)

shortUrl :: String -> String -> Connection -> IO String
shortUrl s l db =
  if s /= ""
    then return s
    else do
      existent <- getNonCustomShortUrl db l
      case existent of
        Nothing -> pickRandomShortUrl db
        Just x -> return x
