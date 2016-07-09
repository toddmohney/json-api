{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Users
  ( User (..)
  , toResource
  ) where

import Data.Aeson.TH
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Network.URL
import Emails (Email)
import qualified Emails as E
import Network.JSONApi.Document
  ( Identifier (..)
  , Links
  , Relationship
  , Resource (..)
  , toLinks
  , mkRelationship
  )

-- Our resource
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

-- helper function to convert a User into a resource object
toResource :: (User, Email) -> Resource User Text
toResource (user, email) =
  Resource resourceId user resourceLinks resourceMetaData relationships
  where
    resourceId       = Identifier (pack . show . userId $ user) "User"
    resourceLinks    = Just $ userLinks user
    resourceMetaData = Nothing
    relationships    = Just $ Map.fromList [ ("email", userEmailRelationship email) ]

-- helper function to build links for a User resource
userLinks :: User -> Links
userLinks user = toLinks [ ("self", selfLink) ]
  where
    selfLink = toURL selfPath
    selfPath = "/users/" <> (show $ userId user)

userEmailRelationship :: Email -> Relationship
userEmailRelationship email =
  fromJust $ mkRelationship
    (Just $ E.mkResourceIdentifer email)
    (Just $ E.mkLinks email)

toURL :: String -> URL
toURL = fromJust . importURL
