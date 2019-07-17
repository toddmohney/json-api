{- |
Module representing a JSON-API resource object.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
module Network.JSONApi.Resource
( Resource (..)
, Relationships
, ResourcefulEntity (..)
, Relationship
, mkRelationship
, mkRelationships
) where

import Control.Lens.TH
import Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Text (Text)
import GHC.Generics hiding (Meta)
import Network.JSONApi.Identifier (HasIdentifier (..), Identifier (..))
import Network.JSONApi.Link (Links)
import Network.JSONApi.Meta (Meta)
import Prelude hiding (id)

{- |
Type representing a JSON-API resource object.

A Resource supplies standardized data and metadata about a resource.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
data Resource a = Resource
  { getIdentifier :: Identifier
  , getResource :: a
  , getLinks :: Maybe Links
  , getRelationships :: Maybe Relationships
  } deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (Resource a) where
  toJSON (Resource (Identifier resId resType metaObj) resObj linksObj rels) =
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
    return $ Resource (Identifier id typ meta) attrs links rels

instance HasIdentifier (Resource a) where
  identifier = getIdentifier

{- |
A typeclass for decorating an entity with JSON API properties
-}
class (ToJSON a, FromJSON a) => ResourcefulEntity a where
  resourceIdentifier :: a -> Text
  resourceType :: a -> Text
  resourceLinks :: a -> Maybe Links
  resourceMetaData :: a -> Maybe Meta
  resourceRelationships :: a -> Maybe Relationships

  fromResource :: Resource a -> a
  fromResource = getResource

  toResource :: a -> Resource a
  toResource a =
    Resource
      (Identifier (resourceIdentifier a) (resourceType a) (resourceMetaData a))
      a
      (resourceLinks a)
      (resourceRelationships a)

{- |
A type representing the Relationship between 2 entities

A Relationship provides basic information for fetching further information
about a related resource.

Specification: <http://jsonapi.org/format/#document-resource-object-relationships>
-}
data Relationship = Relationship
  { _data :: Maybe Identifier
  , _links :: Maybe Links
  } deriving (Show, Eq, Generic)

instance ToJSON Relationship where
  toJSON = AE.genericToJSON
    AE.defaultOptions { AE.fieldLabelModifier = drop 1 }

instance FromJSON Relationship where
  parseJSON = AE.genericParseJSON
    AE.defaultOptions { AE.fieldLabelModifier = drop 1 }


data Relationships = Relationships (Map Text Relationship)
  deriving (Show, Eq, Generic)

instance ToJSON Relationships
instance FromJSON Relationships

instance Semigroup Relationships where
  (<>) (Relationships a) (Relationships b) = Relationships (a <> b)

instance Monoid Relationships where
  mempty = Relationships Map.empty

mkRelationships :: Relationship -> Relationships
mkRelationships rel =
  Relationships $ Map.singleton (relationshipType rel) rel


relationshipType :: Relationship -> Text
relationshipType relationship = case _data relationship of
  Nothing -> "unidentified"
  (Just (Identifier _ typ _)) -> typ


{- |
Constructor function for creating a Relationship record

A relationship must contain either an Identifier or a Links record
-}
mkRelationship :: Maybe Identifier -> Maybe Links -> Maybe Relationship
mkRelationship Nothing Nothing = Nothing
mkRelationship resId links = Just $ Relationship resId links

makeLenses ''Resource
