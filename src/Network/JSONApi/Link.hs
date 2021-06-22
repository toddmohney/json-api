{- |
Module representing a JSON-API link object.

Specification: <http://jsonapi.org/format/#document-links>
-}
module Network.JSONApi.Link
( Links (..)
, Rel
, Href
, mkLinks
) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import qualified GHC.Generics as G
import Network.URL (URL, exportURL)
import Control.DeepSeq (NFData)

{- |
Type representing a JSON-API link object.

Links are an abstraction around an underlying Map consisting of
relevance identifiers as keys and URIs as values.

Example JSON:
@
  "links": {
    "self": "http://example.com/posts/1"
  }
@

Specification: <http://jsonapi.org/format/#document-links>
-}
newtype Links = Links (Map Rel Href)
  deriving (Show, Eq, Ord, ToJSON, FromJSON, G.Generic)

instance NFData Links

type Rel = Text
type Href = Text

{- |
Constructor function for building Links
-}
mkLinks :: [(Rel, URL)] -> Links
mkLinks = Links . Map.fromList . map buildLink

buildLink :: (Rel, URL) -> (Rel, Href)
buildLink (key, url) = (key, pack (exportURL url))
