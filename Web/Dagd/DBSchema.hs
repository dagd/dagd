module Web.Dagd.DBSchema where

import Control.Applicative

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Data.Text (Text)
import Data.Time.LocalTime

data ShortUrl = ShortUrl {
  id :: Integer
, shorturl :: String
, longurl :: Text
, ownerIp :: String
, creationDt :: LocalTime
, enabled :: Maybe Bool
, custom :: Maybe Bool
} deriving Show

instance FromRow ShortUrl where
  fromRow = ShortUrl <$>
            field <*>
            field <*>
            field <*>
            field <*>
            field <*>
            field <*>
            field
