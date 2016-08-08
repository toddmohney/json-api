{- |
Module representing a JSON-API resource object.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
module Network.JSONApi.Resource
( Identifier (..)
, Resource (..)
, ResourcefulEntity (..)
, Relationship
, mkRelationship
) where

import Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import Data.Map (Map)
import Data.Text (Text)
import qualified GHC.Generics as G
import Network.JSONApi.Link (Links)
import Network.JSONApi.Meta (Meta)
import Prelude hiding (id)

class (ToJSON a, FromJSON a) => ResourcefulEntity a where
  resourceIdentifier :: a -> Text
  resourceType :: a -> Text
  resourceLinks :: a -> Maybe Links
  resourceMetaData :: a -> Maybe Meta
  resourceRelationships :: a -> Maybe (Map Text Relationship)

  fromResource :: Resource a -> a
  fromResource = getResource

  toResource :: a -> Resource a
  toResource a =
    Resource
      (Identifier (resourceIdentifier a) (resourceType a))
      a
      (resourceLinks a)
      (resourceMetaData a)
      (resourceRelationships a)

{- |
Type representing a JSON-API resource object.

A Resource supplies standardized data and metadata about a
resource.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
data Resource a = Resource
  { getIdentifier :: Identifier
  , getResource :: a
  , getLinks :: Maybe Links
  , getMetaData :: Maybe Meta
  , getRelationships :: Maybe (Map Text Relationship)
  } deriving (Show, Eq, G.Generic)

instance (ToJSON a) => ToJSON (Resource a) where
  toJSON (Resource (Identifier resId resType) resObj linksObj metaObj rels) =
    AE.object [ "id"            .= resId
              , "type"          .= resType
              , "attributes"    .= resObj
              , "links"         .= linksObj
              , "meta"          .= metaObj
              , "relationships" .= rels
              ]

instance (FromJSON a) => FromJSON (Resource a) where
  parseJSON = AE.withObject "resourceObject" $ \v -> do
    id    <- v .: "id"
    typ   <- v .: "type"
    attrs <- v .: "attributes"
    links <- v .:? "links"
    meta  <- v .:? "meta"
    rels  <- v .:? "relationships"
    return $ Resource (Identifier id typ) attrs links meta rels




{- |
A type representing the Relationship between 2 entities

A Relationship provides basic information for fetching further information
about a related resource.

Specification: <http://jsonapi.org/format/#document-resource-object-relationships>
-}
data Relationship = Relationship
  { _data :: Maybe Identifier
  , _links :: Maybe Links
  } deriving (Show, Eq, G.Generic)

instance ToJSON Relationship where
  toJSON = AE.genericToJSON
    AE.defaultOptions { AE.fieldLabelModifier = drop 1 }

instance FromJSON Relationship where
  parseJSON = AE.genericParseJSON
    AE.defaultOptions { AE.fieldLabelModifier = drop 1 }

{- |
Constructor function for creating a Relationship record

A relationship must contain either an Identifier or a Links record
-}
mkRelationship :: Maybe Identifier -> Maybe Links -> Maybe Relationship
mkRelationship Nothing Nothing = Nothing
mkRelationship resId links = Just $ Relationship resId links



{- |
Identifiers are used to encapsulate the minimum amount of information
to uniquely identify a resource.

This object will be found at multiple levels of the JSON-API structure

Specification: <http://jsonapi.org/format/#document-resource-identifier-objects>
-}
data Identifier = Identifier
  { _id   :: Text
  , _type :: Text
  } deriving (Show, Eq, G.Generic)

instance ToJSON Identifier where
  toJSON = AE.genericToJSON
    AE.defaultOptions { AE.fieldLabelModifier = drop 1 }

instance FromJSON Identifier where
  parseJSON = AE.genericParseJSON
    AE.defaultOptions { AE.fieldLabelModifier = drop 1 }
