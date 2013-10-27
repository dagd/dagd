module Web.Dagd.DB.ShortUrl where

import Control.Applicative

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Data.Text (Text)
import Data.Time.LocalTime

import Text.Blaze (ToMarkup, toMarkup, toValue)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as HA

data ShortUrl = ShortUrl {
  urlId         :: Integer
, urlShorturl   :: String
, urlLongurl    :: Text
, urlOwnerIp    :: String
, urlCreationDt :: LocalTime
, urlEnabled    :: Maybe Bool
, urlCustom     :: Maybe Bool
} deriving (Show, Eq)

instance FromRow ShortUrl where
  fromRow = ShortUrl <$>
            field <*>
            field <*>
            field <*>
            field <*>
            field <*>
            field <*>
            field

instance ToMarkup ShortUrl where
  toMarkup a =
    H.a H.! HA.src (toValue $ urlShorturl a) $ H.toHtml (urlShorturl a)
