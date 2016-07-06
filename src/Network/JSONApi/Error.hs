{- |
Module representing a JSON-API error object.

Error objects are used for providing application-specific detail
to unsuccessful API responses.

Specification: <http://jsonapi.org/format/#error-objects>
-}
module Network.JSONApi.Error
( Error (..)
) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Default
import Data.Swagger (ToSchema)
import Data.Text
import GHC.Generics
import Network.JSONApi.Link (Links)
import Network.JSONApi.Meta
import Prelude hiding (id)

{- |
Type for providing application-specific detail to unsuccessful API
responses.

Specification: <http://jsonapi.org/format/#error-objects>
-}
data Error a =
  Error { id     :: Maybe Text
        , links  :: Maybe Links
        , status :: Maybe Text
        , code   :: Maybe Text
        , title  :: Maybe Text
        , detail :: Maybe Text
        , meta   :: Maybe (Meta a)
        }
  deriving (Show, Eq, Generic)

instance ToJSON a   => ToJSON (Error a)
instance FromJSON a => FromJSON (Error a)
instance ToSchema a => ToSchema (Error a)

instance Default (Error a) where
  def = Error
    { id     = Nothing
    , links  = Nothing
    , status = Nothing
    , code   = Nothing
    , title  = Nothing
    , detail = Nothing
    , meta   = Nothing
    }
