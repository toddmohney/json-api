module Network.JSONApi.Error
( Error (..)
) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Default
import Data.Text
import GHC.Generics
import Network.JSONApi.Link (Links)
import Network.JSONApi.Meta
import Prelude hiding (id)

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