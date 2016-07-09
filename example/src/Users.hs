{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Users
  ( User (..)
  , toResource
  ) where

import Data.Aeson.TH
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Network.URL

import Network.JSONApi.Document
  ( Links
  , Resource (..)
  , ResourceId (..)
  , ResourceType (..)
  , toLinks
  )

-- Our resource
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

-- helper function to convert a User into a resource object
toResource :: User -> Resource User Text
toResource user =
  Resource resourceId resourceType user resourceLinks resourceMetaData
  where
    resourceId       = ResourceId . pack . show . userId $ user
    resourceType     = ResourceType "User"
    resourceLinks    = Just $ userLinks user
    resourceMetaData = Nothing

-- helper function to build links for a User resource
userLinks :: User -> Links
userLinks user = toLinks [ ("self", selfLink) ]
  where
    selfLink = toURL selfPath
    selfPath = "/users/" <> (show $ userId user)

toURL :: String -> URL
toURL = fromJust . importURL
