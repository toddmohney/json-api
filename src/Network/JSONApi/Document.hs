module Network.JSONApi.Document
( Document (..)
, RO.ResourceId (..)
, RO.ResourceObject (..)
, RO.ResourceType (..)
, L.Links
, M.Meta (..)
, L.toLinks
) where

import           Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import           Data.Swagger (ToSchema)
import           GHC.Generics
import qualified Network.JSONApi.Document.Success as Doc
import           Network.JSONApi.Link as L
import           Network.JSONApi.Meta as M
import qualified Network.JSONApi.ResourceObject as RO

data Document a b c = Resource (Doc.Success a b c)
                   deriving (Show, Eq, Generic)

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (Document a b c) where
  toJSON (Resource (Doc.Success res links meta)) =
    AE.object [ "data"  .= res
              , "links" .= links
              , "meta"  .= meta
              ]

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (Document a b c) where
  parseJSON = AE.withObject "document" $ \v -> do
    d <- v .:  "data"
    l <- v .:? "links"
    m <- v .:? "meta"
    return (Resource $ Doc.Success d l m)

instance (ToSchema a, ToSchema b, ToSchema c) => ToSchema (Document a b c)
