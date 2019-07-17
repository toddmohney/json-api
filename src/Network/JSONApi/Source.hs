{- |
Module representing a JSON-API Source object.

Specification: <https://jsonapi.org/format/#error-objects>
               <https://tools.ietf.org/html/rfc6901>
-}
module Network.JSONApi.Source (
  Source (..)
) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Default (Default, def)
import Data.Text (Text)
import qualified GHC.Generics as G

data Source =
  Source { pointer :: Maybe Text
         , parameter :: Maybe Text
         }
  deriving (Show, Eq, G.Generic)

instance ToJSON Source
instance FromJSON Source

instance Default Source where
  def = Source
    { pointer = Nothing
    , parameter = Nothing
    }