module Network.JSONApi.Document.Error
  ( Error (..)
  ) where

import Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import qualified Network.JSONApi.Error as E
import Network.JSONApi.Link (Links)
import Network.JSONApi.Meta (Meta)

data Error a b = Error
  { getError :: E.Error a
  , getLinks :: Maybe Links
  , getMeta  :: Maybe (Meta b)
  } deriving (Show, Eq)

instance (ToJSON a, ToJSON b) => ToJSON (Error a b) where
  toJSON (Error err links meta) =
    AE.object [ "error" .= err
              , "links" .= links
              , "meta"  .= meta
              ]

instance (FromJSON a, FromJSON b) => FromJSON (Error a b) where
  parseJSON = AE.withObject "error" $ \v ->
    Error
      <$> v .: "error"
      <*> v .:? "links"
      <*> v .:? "meta"
