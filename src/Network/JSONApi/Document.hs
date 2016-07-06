{- |
Entry-point module for this package.

Contains representations of the top-level JSON-API document structure.
-}
module Network.JSONApi.Document
( Document (..)
, ErrorDocument (..)
, E.Error (..)
, Resource (..)
, RO.ResourceId (..)
, RO.ResourceObject (..)
, RO.ResourceType (..)
, L.Links
, M.Meta (..)
, L.toLinks
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
import Data.Swagger (ToSchema)
import GHC.Generics
import qualified Network.JSONApi.Error as E
import Network.JSONApi.Link as L
import Network.JSONApi.Meta as M
import Network.JSONApi.ResourceObject (ResourceObject)
import qualified Network.JSONApi.ResourceObject as RO

{- |
The @Document@ type represents the top-level JSON-API requirement.

@data@ attribute - the resulting JSON may be either a singleton resource
or a list of resources. See 'Resource' for the construction.

For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data Document a b c = Document
  { _data  ::  Resource a b
  , _links ::  Maybe Links
  , _meta  ::  Maybe (Meta c)
  } deriving (Show, Eq, Generic)

{- |
The @Resource@ type encapsulates the underlying 'ResourceObject'

Included in the top-level 'Document', the @Resource@ may be either
a singleton resource or a list.

For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data Resource a b = Singleton (ResourceObject a b)
                  | List [ResourceObject a b]
                  deriving (Show, Eq, Generic)

instance (ToJSON a, ToJSON b) => ToJSON (Resource a b) where
  toJSON (Singleton res) = AE.toJSON res
  toJSON (List res)      = AE.toJSON res

instance (FromJSON a, FromJSON b) => FromJSON (Resource a b) where
  parseJSON (AE.Object v) = Singleton <$> (AE.parseJSON (AE.Object v))
  parseJSON (AE.Array v)  = List <$> (AE.parseJSON (AE.Array v))
  parseJSON _             = mzero

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (Document a b c) where
  toJSON (Document (List res) links meta) =
    AE.object [ "data"  .= res
              , "links" .= links
              , "meta"  .= meta
              ]
  toJSON (Document (Singleton res) links meta) =
    AE.object [ "data"  .= res
              , "links" .= links
              , "meta"  .= meta
              ]

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (Document a b c) where
  parseJSON = AE.withObject "document" $ \v -> do
    d <- v .:  "data"
    l <- v .:? "links"
    m <- v .:? "meta"
    return (Document d l m)

instance (ToSchema a, ToSchema b, ToSchema c) => ToSchema (Document a b c)
instance (ToSchema a, ToSchema b) => ToSchema (Resource a b)

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
  } deriving (Show, Eq, Generic)

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

instance (ToSchema a, ToSchema b) => ToSchema (ErrorDocument a b)
