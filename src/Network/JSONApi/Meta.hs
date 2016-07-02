module Network.JSONApi.Meta where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.Map (Map)
import           Data.Text (Text)
import           GHC.Generics

data Meta a = Meta (Map Text a)
  deriving (Show, Eq, Ord, Generic)

instance ToJSON a   => ToJSON (Meta a)
instance FromJSON a => FromJSON (Meta a)

