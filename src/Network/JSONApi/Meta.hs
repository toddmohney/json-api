{- |
Module representing a JSON-API meta object.

Specification: <http://jsonapi.org/format/#document-meta>
-}
module Network.JSONApi.Meta where

import Data.Aeson (ToJSON, FromJSON)
import Data.Map (Map)
import Data.Text (Text)
import qualified GHC.Generics as G

{- |
Type representing a JSON-API meta object.

Meta is an abstraction around an underlying Map consisting of
resource-specific metadata.

Example JSON:
@
"meta": {
  "copyright": "Copyright 2015 Example Corp.",
  "authors": [
    "Andre Dawson",
    "Kirby Puckett",
    "Don Mattingly",
    "Ozzie Guillen"
  ]
}
@

Specification: <http://jsonapi.org/format/#document-meta>
-}
data Meta a = Meta (Map Text a)
  deriving (Show, Eq, Ord, G.Generic)

instance ToJSON a   => ToJSON (Meta a)
instance FromJSON a => FromJSON (Meta a)
