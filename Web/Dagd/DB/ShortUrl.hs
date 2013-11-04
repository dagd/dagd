module Web.Dagd.DB.ShortUrl where

import Control.Applicative

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow


import Data.Text (Text)
import Data.Time.LocalTime

import Text.Blaze (ToMarkup, toMarkup, toValue)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as HA

data ShortUrl = ShortUrl {
  urlId         :: Maybe Integer
, urlShorturl   :: String
, urlLongurl    :: Text
, urlOwnerIp    :: String
, urlCreationDt :: Maybe LocalTime
, urlEnabled    :: Bool -- Requires a schema change to not null
, urlCustom     :: Bool -- Requires a schema change to not null
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

instance ToRow ShortUrl where
  toRow u = [ toField (urlShorturl u)
            , toField (urlLongurl u)
            , toField (urlOwnerIp u)
            , toField (urlEnabled u)
            , toField (urlCustom u) ]

instance ToMarkup ShortUrl where
  toMarkup a =
    H.a H.! HA.src (toValue $ urlShorturl a) $ H.toHtml (urlShorturl a)
