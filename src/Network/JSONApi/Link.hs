module Network.JSONApi.Link
( Links
, Rel
, Href
, toLinks
) where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Swagger (ToSchema)
import           Data.Text (Text, pack)
import           GHC.Generics
import           Network.URL (URL, exportURL)

newtype Links = Links (Map Rel Href)
  deriving (Show, Eq, Ord, ToJSON, FromJSON, Generic)

instance ToSchema Links

type Rel = Text
type Href = Text

toLinks :: [(Rel, URL)] -> Links
toLinks = Links . Map.fromList . map buildLink

buildLink :: (Rel, URL) -> (Rel, Href)
buildLink (key, url) = (key, pack (exportURL url))
