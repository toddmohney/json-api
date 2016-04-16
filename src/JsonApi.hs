{-# LANGUAGE OverloadedStrings #-}

module JsonApi
( JsonApi (..)
, RO.ResourceId (..)
, RO.ResourceObject (..)
, RO.ResourceType (..)
, RO.ToResourceObject (..)
) where

import           Control.Monad (mzero)
import           Data.Aeson ((.=), (.:), (.:?))
import qualified Data.Aeson as AE
import           ResourceObject (ResourceObject)
import qualified ResourceObject as RO

data JsonApi a b = JsonApi (ResourceObject a) (Maybe b)
  deriving (Show, Eq, Ord)

instance (AE.ToJSON a, AE.ToJSON b) => AE.ToJSON (JsonApi a b) where
  toJSON (JsonApi resObj (Just metaObj)) =
    AE.object [ "data" .= resObj
              , "meta" .= metaObj
              ]
  toJSON (JsonApi resObj Nothing) =
    AE.object [ "data" .= resObj
              ]

instance (AE.FromJSON a, AE.FromJSON b) => AE.FromJSON (JsonApi a b) where
  parseJSON (AE.Object v) =
    JsonApi
      <$> v .: "data"
      <*> v .:? "meta"
  parseJSON _ = mzero
