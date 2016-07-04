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
import qualified Data.HashMap.Strict as HM
import           Network.JSONApi.Document.Error as Doc
import           Network.JSONApi.Document.Success as Doc
import           Network.JSONApi.Error as E
import           Network.JSONApi.Link as L
import           Network.JSONApi.Meta as M
import           Network.JSONApi.ResourceObject (ResourceObject)
import qualified Network.JSONApi.ResourceObject as RO

data JsonApi a b c = SuccessApi (Doc.Success a b c)
                   | ErrorApi (Doc.Error a b)
                   deriving (Show, Eq)

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (JsonApi a b c) where
  toJSON (SuccessApi (Doc.Success res links meta)) =
    AE.object [ "data"  .= res
              , "links" .= links
              , "meta"  .= meta
              ]
  toJSON (ErrorApi (Doc.Error err links meta)) =
    AE.object [ "error" .= err
              , "links" .= links
              , "meta"  .= meta
              ]

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (JsonApi a b c) where
  parseJSON obj@(AE.Object v) =
    case HM.lookup "error" v of
      Nothing -> do
        d <- v .:  "data"
        l <- v .:? "links"
        m <- v .:? "meta"
        return (SuccessApi $ Doc.Success d l m)
      (Just _) -> do
        e <- v .:  "error"
        l <- v .:? "links"
        m <- v .:? "meta"
        return (ErrorApi $ Doc.Error e l m)
  parseJSON _ = mzero
