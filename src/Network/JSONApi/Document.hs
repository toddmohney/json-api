{- |
Entry-point module for this package.

Contains representations of the top-level JSON-API document structure.
-}
module Network.JSONApi.Document
( Document (..)
, ErrorDocument (..)
, E.Error (..)
, ResourceData (..)
, R.Relationship
, R.Resource (..)
, R.Identifier (..)
, L.Links
, M.Meta (..)
, L.toLinks
, R.mkRelationship
, mkDocument
) where

import Control.Monad (mzero)
import Data.Aeson
  ( ToJSON
  , FromJSON
  , (.=)
  , (.:)
  , (.:?)
  )
import qualified Data.Aeson as AE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified GHC.Generics as G
import qualified Network.JSONApi.Error as E
import Network.JSONApi.Link as L
import Network.JSONApi.Meta as M
import Network.JSONApi.Resource (Resource, ResourcefulEntity)
import qualified Network.JSONApi.Resource as R

{- |
The @Document@ type represents the top-level JSON-API requirement.

@data@ attribute - the resulting JSON may be either a singleton resource
or a list of resources. See 'Resource' for the construction.

For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data Document a b c = Document
  { _data  ::  ResourceData a b
  , _links ::  Maybe Links
  , _meta  ::  Maybe (Meta c)
  , _included :: [ Map Text (Resource a b) ]
  } deriving (Show, Eq, G.Generic)

instance (ToJSON a, ToJSON b, ToJSON c)
      => ToJSON (Document a b c) where
  toJSON (Document (List res) links meta included) =
    AE.object [ "data"  .= res
              , "links" .= links
              , "meta"  .= meta
              , "included" .= included
              ]
  toJSON (Document (Singleton res) links meta included) =
    AE.object [ "data"  .= res
              , "links" .= links
              , "meta"  .= meta
              , "included" .= included
              ]

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (Document a b c) where
  parseJSON = AE.withObject "document" $ \v -> do
    d <- v .:  "data"
    l <- v .:? "links"
    m <- v .:? "meta"
    i <- v .: "included"
    return (Document d l m i)

mkDocument :: ResourcefulEntity a =>
              [a]
           -> Maybe Links
           -> Maybe (Meta c)
           -> [a]
           -> Document a b c
mkDocument res links meta included =
  Document (toResourceData res) links meta (toIncludedResources included)

toResourceData :: ResourcefulEntity a => [a] -> ResourceData a b
toResourceData (r:[]) = Singleton (R.toResource r)
toResourceData rs      = List (map R.toResource rs)

toIncludedResources :: ResourcefulEntity a => [a] -> [ Map Text (Resource a b) ]
toIncludedResources entities =
  foldl (\acc ent -> acc ++ [Map.singleton (R.resourceType ent) (R.toResource ent)]) [] entities

{- |
The @Resource@ type encapsulates the underlying 'Resource'

Included in the top-level 'Document', the @Resource@ may be either
a singleton resource or a list.

For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data ResourceData a b = Singleton (Resource a b)
                      | List [Resource a b]
                      deriving (Show, Eq, G.Generic)

instance (ToJSON a, ToJSON b) => ToJSON (ResourceData a b) where
  toJSON (Singleton res) = AE.toJSON res
  toJSON (List res)      = AE.toJSON res

instance (FromJSON a, FromJSON b) => FromJSON (ResourceData a b) where
  parseJSON (AE.Object v) = Singleton <$> (AE.parseJSON (AE.Object v))
  parseJSON (AE.Array v)  = List <$> (AE.parseJSON (AE.Array v))
  parseJSON _             = mzero

{- |
The @ErrorDocument@ type represents the alternative form of the top-level
JSON-API requirement.

@error@ attribute - a descriptive object encapsulating application-specific
error detail.

For more information see: <http://jsonapi.org/format/#errors>
-}
data ErrorDocument a b = ErrorDocument
  { _error :: E.Error a
  , _errorLinks :: Maybe Links
  , _errorMeta  :: Maybe (Meta b)
  } deriving (Show, Eq, G.Generic)

instance (ToJSON a, ToJSON b) => ToJSON (ErrorDocument a b) where
  toJSON (ErrorDocument err links meta) =
    AE.object [ "error" .= err
              , "links" .= links
              , "meta"  .= meta
              ]

instance (FromJSON a, FromJSON b) => FromJSON (ErrorDocument a b) where
  parseJSON = AE.withObject "error" $ \v ->
    ErrorDocument
      <$> v .: "error"
      <*> v .:? "links"
      <*> v .:? "meta"
