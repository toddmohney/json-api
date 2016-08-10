{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Users
  ( User (..)
  , toResource
  ) where

import Data.Aeson.TH
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (pack)
import Network.URL
import Network.JSONApi
  ( Links
  , ResourcefulEntity (..)
  )
import qualified Network.JSONApi as JSONApi

-- Our resource
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

instance ResourcefulEntity User where
  resourceIdentifier = pack . show . userId
  resourceType _ = "User"
  resourceLinks = Just . userLinks
  resourceMetaData _ = Nothing
  resourceRelationships _ = Nothing

-- helper function to build links for a User resource
userLinks :: User -> Links
userLinks user = JSONApi.mkLinks [ ("self", selfLink) ]
  where
    selfLink = toURL selfPath
    selfPath = "/users/" <> (show $ userId user)

toURL :: String -> URL
toURL = fromJust . importURL
