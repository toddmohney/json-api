{- |
Module representing a JSON-API resource object.

Specification: <http://jsonapi.org/format/#document-resource-objects>
-}
module Network.JSONApi.Identifier
( HasIdentifier (..)
, Identifier (..)
, datatype
, ident
, metadata
) where

import Control.Lens.TH
import Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import qualified Data.Aeson as AE
import Data.Text (Text)
import Network.JSONApi.Meta (Meta)
import Prelude hiding (id)

{- |
Identifiers are used to encapsulate the minimum amount of information
to uniquely identify a resource.

This object will be found at multiple levels of the JSON-API structure

Specification: <http://jsonapi.org/format/#document-resource-identifier-objects>
-}
data Identifier = Identifier
  { _ident :: Text
  , _datatype :: Text
  , _metadata :: Maybe Meta
  } deriving (Show, Eq)

instance ToJSON Identifier where
  toJSON (Identifier resId resType resMetaData) =
    AE.object [ "id"            .= resId
              , "type"          .= resType
              , "meta"          .= resMetaData
              ]

instance FromJSON Identifier where
  parseJSON = AE.withObject "resourceIdentifier" $ \v -> do
    id    <- v .: "id"
    typ   <- v .: "type"
    meta  <- v .:? "meta"
    return $ Identifier id typ meta


{- |
Typeclass indicating how to access an 'Identifier' for
a given datatype
-}
class HasIdentifier a where
  identifier :: a -> Identifier

makeLenses ''Identifier
