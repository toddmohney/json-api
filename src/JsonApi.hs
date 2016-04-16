{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonApi
( JsonApi (..)
, RO.ResourceId (..)
, RO.ResourceObject (..)
, RO.ResourceType (..)
, RO.ToResourceObject (..)
) where

import           Control.Monad (mzero)
import           Data.Aeson ((.=), (.:))
import qualified Data.Aeson as AE
import           ResourceObject (ResourceObject)
import qualified ResourceObject as RO

data JsonApi a = JsonApi (ResourceObject a)
  deriving (Show, Eq, Ord)

instance (AE.ToJSON a) => AE.ToJSON (JsonApi a) where
  toJSON (JsonApi resObj) =
    AE.object [ "data" .= resObj ]

instance (AE.FromJSON a) => AE.FromJSON (JsonApi a) where
  parseJSON (AE.Object v) = JsonApi <$> v .: "data"
  parseJSON _             = mzero
