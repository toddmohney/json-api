module Network.JsonApi
( JsonApi (..)
, RO.ResourceId (..)
, RO.ResourceObject (..)
, RO.ResourceType (..)
, L.Links
, M.Meta
, L.toLinks
) where

import           Control.Monad (mzero)
import           Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import           Network.Link as L
import           Network.Meta as M
import           Network.ResourceObject (ResourceObject)
import qualified Network.ResourceObject as RO

data JsonApi a b c =
  JsonApi (ResourceObject a b) (Maybe Links) (Maybe (Meta c))
    deriving (Show, Eq, Ord)

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (JsonApi a b c) where
  toJSON (JsonApi res links meta) =
    AE.object [ "data"  .= res
              , "links" .= links
              , "meta"  .= meta
              ]

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (JsonApi a b c) where
  parseJSON (AE.Object v) =
    JsonApi
      <$> v .: "data"
      <*> v .:? "links"
      <*> v .:? "meta"
  parseJSON _ = mzero
