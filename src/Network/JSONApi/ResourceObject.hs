{- |
Module representing a JSON-API resource object.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
module Network.JSONApi.ResourceObject
( ResourceId (..)
, ResourceObject (..)
, ResourceType (..)
) where

import           Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import           Data.Swagger (ToSchema)
import           Data.Text (Text)
import           GHC.Generics
import           Network.JSONApi.Link (Links)
import           Network.JSONApi.Meta (Meta)

{- |
Type representing a JSON-API resource object.

A ResourceObject supplies standardized data and metadata about a
resource.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
data ResourceObject a b = ResourceObject
  { getResourceId :: ResourceId
  , getResourceType :: ResourceType
  , getResource :: a
  , getLinks :: Maybe Links
  , getMetaData :: Maybe (Meta b)
  } deriving (Show, Eq, Ord, Generic)

newtype ResourceId = ResourceId Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON, Generic)

newtype ResourceType = ResourceType Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON, Generic)

instance (ToSchema a, ToSchema b) => ToSchema (ResourceObject a b)
instance ToSchema ResourceId
instance ToSchema ResourceType

instance (ToJSON a, ToJSON b) => ToJSON (ResourceObject a b) where
  toJSON (ResourceObject resId resType resObj linksObj metaObj) =
    AE.object [ "id"         .= resId
              , "type"       .= resType
              , "attributes" .= resObj
              , "links"      .= linksObj
              , "meta"       .= metaObj
              ]

instance (FromJSON a, FromJSON b) => FromJSON (ResourceObject a b) where
  parseJSON = AE.withObject "resourceObject" $ \v ->
                ResourceObject
                  <$> v .: "id"
                  <*> v .: "type"
                  <*> v .: "attributes"
                  <*> v .:? "links"
                  <*> v .:? "meta"
