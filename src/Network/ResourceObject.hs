{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.ResourceObject
( ResourceId (..)
, ResourceObject (..)
, ResourceType (..)
, ToResourceObject (..)
) where

import           Control.Monad (mzero)
import           Data.Aeson (ToJSON, FromJSON, (.=), (.:))
import qualified Data.Aeson as AE
import           Data.Text (Text)

class (ToJSON a) => ToResourceObject a where
  toResource :: a -> ResourceObject a

data ResourceObject a = ResourceObject ResourceId ResourceType a
  deriving (Show, Eq, Ord)

newtype ResourceId = ResourceId Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

newtype ResourceType = ResourceType Text
  deriving (Show, Eq, Ord, ToJSON, FromJSON)

instance (ToJSON a) => ToJSON (ResourceObject a) where
  toJSON (ResourceObject resId resType resObj) =
    AE.object [ "id"         .= resId
              , "type"       .= resType
              , "attributes" .= resObj
              ]

instance (FromJSON a) => FromJSON (ResourceObject a) where
  parseJSON (AE.Object v) = ResourceObject <$>
                              v .: "id" <*>
                              v .: "type" <*>
                              v .: "attributes"
  parseJSON _          = mzero
