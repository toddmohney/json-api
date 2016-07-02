module Network.JSONApi.JsonApi
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
import           Network.JSONApi.Error as E
import           Network.JSONApi.Link as L
import           Network.JSONApi.Meta as M
import           Network.JSONApi.ResourceObject (ResourceObject)
import qualified Network.JSONApi.ResourceObject as RO

data JsonApi a b c = SuccessApi (ResourceObject a b) (Maybe Links) (Maybe (Meta c))
                   | ErrorApi (Error a) (Maybe Links) (Maybe (Meta b))
                   deriving (Show, Eq)

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (JsonApi a b c) where
  toJSON (SuccessApi res links meta) =
    AE.object [ "data"  .= res
              , "links" .= links
              , "meta"  .= meta
              ]
  toJSON (ErrorApi err links meta) =
    AE.object [ "error" .= err
              , "links" .= links
              , "meta"  .= meta
              ]

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (JsonApi a b c) where
  parseJSON (AE.Object v) =
    SuccessApi
      <$> v .: "data"
      <*> v .:? "links"
      <*> v .:? "meta"
  parseJSON _ = mzero
