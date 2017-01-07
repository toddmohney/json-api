{- |
Contains representations of the top-level JSON-API document structure.
-}
module Network.JSONApi.Document
  ( Document
  , ResourceData (..)
  , ErrorDocument (..)
  , Included
  , mkDocument
  , mkDocument'
  , singleton
  , list
  , mkCompoundDocument
  , mkCompoundDocument'
  , mkIncludedResource
  ) where

import Control.Monad (mzero)
import Data.Aeson
  ( ToJSON
  , FromJSON
  , Value
  , (.=)
  , (.:)
  , (.:?)
  )
import qualified Data.Aeson as AE
import Data.Monoid
import qualified GHC.Generics as G
import qualified Network.JSONApi.Error as E
import Network.JSONApi.Link as L
import Network.JSONApi.Meta as M
import Network.JSONApi.Resource (Resource, ResourcefulEntity)
import qualified Network.JSONApi.Resource as R

{- |
The 'Document' type represents the top-level JSON-API requirement.

@data@ attribute - the resulting JSON may be either a singleton resource
or a list of resources. See 'Resource' for the construction.

For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data Document a = Document
  { _data  ::  ResourceData a
  , _links ::  Maybe Links
  , _meta  ::  Maybe Meta
  , _included :: [Value]
  } deriving (Show, Eq, G.Generic)

instance (ToJSON a)
      => ToJSON (Document a) where
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

instance (FromJSON a) => FromJSON (Document a) where
  parseJSON = AE.withObject "document" $ \v -> do
    d <- v .:  "data"
    l <- v .:? "links"
    m <- v .:? "meta"
    i <- v .: "included"
    return (Document d l m i)

{- |
The 'Included' type is an abstraction used to constrain the @included@
section of the Document to JSON serializable Resource objects while
enabling a heterogeneous list of Resource types.

No data constructors for this type are exported as we need to
constrain the 'Value' to a heterogeneous list of Resource types.
See 'mkIncludedResource' for creating 'Included' types.
-}
data Included = Included [Value]
  deriving (Show)

instance Monoid Included where
  mempty = Included []
  mappend (Included as) (Included bs) = Included (as <> bs)

{- |
Constructor function for the Document data type.

See 'mkCompoundDocument' for constructing compound Document
including 'side-loaded' resources
-}
mkDocument :: ResourcefulEntity a =>
              [a]
           -> Maybe Links
           -> Maybe Meta
           -> Document a
mkDocument res = mkDocument' (toResourceData res)

mkDocument' :: ResourceData a
            -> Maybe Links
            -> Maybe Meta
            -> Document a
mkDocument' res links meta =
  Document
    { _data = res
    , _links = links
    , _meta = meta
    , _included = []
    }

{- |
Constructor function for the Document data type.
See 'mkIncludedResource' for constructing the 'Included' type.

Supports building compound documents
<http://jsonapi.org/format/#document-compound-documents>
-}
mkCompoundDocument :: ResourcefulEntity a =>
                      [a]
                   -> Maybe Links
                   -> Maybe Meta
                   -> Included
                   -> Document a
mkCompoundDocument res = mkCompoundDocument' (toResourceData res)

mkCompoundDocument' :: ResourceData a
                    -> Maybe Links
                    -> Maybe Meta
                    -> Included
                    -> Document a
mkCompoundDocument' res links meta (Included included) =
  Document
    { _data = res
    , _links = links
    , _meta = meta
    , _included = included
    }

{- |
Constructor function for the Document data type.

Supports building compound documents
<http://jsonapi.org/format/#document-compound-documents>
-}
mkIncludedResource :: ResourcefulEntity a => a -> Included
mkIncludedResource res = Included [AE.toJSON . R.toResource $ res]

toResourceData :: ResourcefulEntity a => [a] -> ResourceData a
toResourceData (r:[]) = Singleton (R.toResource r)
toResourceData rs     = List (map R.toResource rs)

{- |
The 'Resource' type encapsulates the underlying 'Resource'

Included in the top-level 'Document', the 'Resource' may be either
a singleton resource or a list.

For more information see: <http://jsonapi.org/format/#document-top-level>
-}
data ResourceData a = Singleton (Resource a)
                    | List [ Resource a ]
                    deriving (Show, Eq, G.Generic)

singleton :: ResourcefulEntity a => a -> ResourceData a
singleton = Singleton . R.toResource

list :: ResourcefulEntity a => [a] -> ResourceData a
list = List . map R.toResource

instance (ToJSON a) => ToJSON (ResourceData a) where
  toJSON (Singleton res) = AE.toJSON res
  toJSON (List res)      = AE.toJSON res

instance (FromJSON a) => FromJSON (ResourceData a) where
  parseJSON (AE.Object v) = Singleton <$> (AE.parseJSON (AE.Object v))
  parseJSON (AE.Array v)  = List <$> (AE.parseJSON (AE.Array v))
  parseJSON _             = mzero

{- |
The 'ErrorDocument' type represents the alternative form of the top-level
JSON-API requirement.

@error@ attribute - a descriptive object encapsulating application-specific
error detail.

For more information see: <http://jsonapi.org/format/#errors>
-}
data ErrorDocument a = ErrorDocument
  { _error :: E.Error a
  , _errorLinks :: Maybe Links
  , _errorMeta  :: Maybe Meta
  } deriving (Show, Eq, G.Generic)

instance (ToJSON a) => ToJSON (ErrorDocument a) where
  toJSON (ErrorDocument err links meta) =
    AE.object [ "error" .= err
              , "links" .= links
              , "meta"  .= meta
              ]

instance (FromJSON a) => FromJSON (ErrorDocument a) where
  parseJSON = AE.withObject "error" $ \v ->
    ErrorDocument
      <$> v .: "error"
      <*> v .:? "links"
      <*> v .:? "meta"
