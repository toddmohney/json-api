module Network.JSONApi.ResourceObject
( ResourceId (..)
, ResourceObject (..)
, ResourceType (..)
) where

import           Control.Monad (mzero)
import           Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import           Data.Text (Text)
import           Network.JSONApi.Link (Links)
import           Network.JSONApi.Meta (Meta)

data ResourceObject a b = ResourceObject
  { getResourceId :: ResourceId
  , getResourceType :: ResourceType
  , getResource :: a
  , getLinks :: Maybe Links
  , getMetaData :: Maybe (Meta b)
  } deriving (Show, Eq, Ord)

newtype ResourceId = ResourceId Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

newtype ResourceType = ResourceType Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

instance (ToJSON a, ToJSON b) => ToJSON (ResourceObject a b) where
  toJSON (ResourceObject resId resType resObj linksObj metaObj) =
    AE.object [ "id"         .= resId
              , "type"       .= resType
              , "attributes" .= resObj
              , "links"      .= linksObj
              , "meta"       .= metaObj
              ]

instance (FromJSON a, FromJSON b) => FromJSON (ResourceObject a b) where
  parseJSON (AE.Object v) = ResourceObject
                              <$> v .: "id"
                              <*> v .: "type"
                              <*> v .: "attributes"
                              <*> v .:? "links"
                              <*> v .:? "meta"
  parseJSON _          = mzero

