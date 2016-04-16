{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonApi
( ResourceId (..)
, ResourceObject (..)
, ResourceType (..)
, ToResourceObject (..)
) where

import Control.Monad (mzero)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as AE
import Data.Text (Text)

class (AE.ToJSON a) => ToResourceObject a where
  toResource :: a -> ResourceObject a

newtype ResourceId = ResourceId Text
  deriving (Show, Eq, Ord, AE.ToJSON, AE.FromJSON)

newtype ResourceType = ResourceType Text
  deriving (Show, Eq, Ord, AE.ToJSON, AE.FromJSON)

data ResourceObject a = ResourceObject ResourceId ResourceType a
  deriving (Show, Eq, Ord)

instance (AE.ToJSON a) => AE.ToJSON (ResourceObject a) where
  toJSON (ResourceObject resId resType resObj) =
    AE.object [ "id"         .= resId
              , "type"       .= resType
              , "attributes" .= resObj
              ]

instance (AE.FromJSON a) => AE.FromJSON (ResourceObject a) where
  parseJSON (AE.Object v) = ResourceObject <$>
                              v .: "id" <*>
                              v .: "type" <*>
                              v .: "attributes"
  parseJSON _          = mzero

