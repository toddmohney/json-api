{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ResourceObject
( ResourceId (..)
, ResourceObject (..)
, ResourceType (..)
) where

import           Control.Monad (mzero)
import           Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import           Data.Text (Text)
import           Network.Meta (Meta)

data ResourceObject a b = ResourceObject
  { getResourceId :: ResourceId
  , getResourceType :: ResourceType
  , getResource :: a
  , getMetaData :: Maybe (Meta b)
  } deriving (Show, Eq, Ord)

newtype ResourceId = ResourceId Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

newtype ResourceType = ResourceType Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

instance (ToJSON a, ToJSON b) => ToJSON (ResourceObject a b) where
  toJSON (ResourceObject resId resType resObj metaObj) =
    AE.object [ "id"         .= resId
              , "type"       .= resType
              , "attributes" .= resObj
              , "meta"       .= metaObj
              ]

instance (FromJSON a, FromJSON b) => FromJSON (ResourceObject a b) where
  parseJSON (AE.Object v) = ResourceObject
                              <$> v .: "id"
                              <*> v .: "type"
                              <*> v .: "attributes"
                              <*> v .:? "meta"
  parseJSON _          = mzero

